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

mod asm_constraints;
mod asm_templates;
mod cast_to_union;
mod complex_specifiers;
mod conditional_operands;
mod declarations;
mod expressions;
mod function_compatibility;
mod incomplete_types;
mod inline_static_reference;
mod return_conversion;
mod va_arg_pack;

use crate::common::{
    compile_and_run, compile_and_run_two_units, compile_expect_error, compile_expect_no_diagnostic,
    compile_expect_ok, compile_expect_warning, create_c_file, run_c17,
};

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

/// A stray `;` at file scope is an empty declaration, not a declaration with a
/// missing type specifier. It reached `check_implicit_int` and was rejected
/// with a wrong message, which broke any source using a function-like macro
/// that expands to nothing -- CPython's `_Py_DECLARE_STR()` is one, and this
/// failed the CPython acceptance build.
///
/// C17 6.7p2 does make it a constraint violation, but GCC and Clang accept it
/// by default and warn only under -pedantic, so accepting it is what real
/// source expects.
#[test]
fn diagnostics_empty_declaration_is_accepted() {
    for (name, src) in [
        ("empty_decl_bare", ";\nint main(void){return 0;}\n"),
        ("empty_decl_repeated", ";;;\nint main(void){return 0;}\n"),
        (
            "empty_decl_between_declarations",
            "int a;\n;\nint b;\nint main(void){return 0;}\n",
        ),
        (
            "empty_decl_from_empty_macro",
            "#define DECLARE(x)\nDECLARE(thing);\nint main(void){return 0;}\n",
        ),
        (
            "empty_decl_after_function",
            "int f(void){return 0;};\nint main(void){return f();}\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
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

/// C17 6.7.7p3 admits a typedef of a variably modified type at block scope,
/// and c17 used to reject one outright. The rejection was a limitation, not a
/// rule -- see `cc/tests/c99/types.rs` for the semantics it now implements.
///
/// What 6.7.7p3 does forbid is such a typedef at *file* scope, where there is
/// no order of execution to evaluate the extent in.
#[test]
fn diagnostics_vm_typedef_is_block_scope_only() {
    compile_expect_ok(
        "typedef_vla_block_scope",
        "int main(void){int n=4; typedef int arr_t[n]; arr_t x; x[0]=1; return x[0]-1;}\n",
    );
    // The file-scope spelling is refused by the declarator, before the
    // typedef branch is reached -- the same message gcc's "variably modified
    // 'T' at file scope" carries, and the same one a plain `int a[n];` at file
    // scope gets (#C88).
    compile_expect_error(
        "typedef_vla_file_scope",
        "int n = 4;\ntypedef int arr_t[n];\nint main(void){ return 0; }\n",
        "cannot have file scope",
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
// #C88 — a variably modified declaration at file scope (C17 6.7.6.2p2)
// ============================================================================

/// 6.7.6.2p2 confines an ordinary identifier with a variably modified type to
/// block scope. c17 accepted one at file scope and sized it **zero**: the
/// first file-scope declarator has its own array-dimension loop, separate from
/// `parse_declarator`, and it mapped every non-constant dimension to
/// `unwrap_or(0)` without recording a VLA or saying anything. So `int bad[n];`
/// compiled, `sizeof bad` was 0, and every access ran off the end.
///
/// The three checks that already existed reached only the grouped-declarator,
/// K&R and second-declarator paths, which is why a plain `int bad[n];` walked
/// past all of them.
#[test]
fn diagnostics_variably_modified_declaration_at_file_scope_is_rejected() {
    for (name, src) in [
        ("fs_vla", "int n;\nint bad[n];\n"),
        ("fs_vla_static", "int n;\nstatic int bad[n];\n"),
        ("fs_vla_extern", "int n;\nextern int bad[n];\n"),
        ("fs_vla_2d", "int n;\nint bad[n][2];\n"),
        ("fs_vla_inner", "int n;\nint bad[2][n];\n"),
        ("fs_vla_expr", "int n;\nint bad[n + 1];\n"),
        ("fs_vla_call", "int f(void);\nint bad[f()];\n"),
        // Second and later declarators were already caught; pinned so the
        // first-declarator fix does not become the only path that checks.
        ("fs_vla_second", "int n;\nint ok[2], bad[n];\n"),
        // Through a `sizeof` of a variably modified type-name, which is not a
        // constant expression either (#C52).
        ("fs_vla_sizeof", "int n;\nint bad[sizeof(int[n])];\n"),
    ] {
        compile_expect_error(name, src, "file scope");
    }
}

/// A size that is a constant but not positive is a different constraint
/// (6.7.6.2p1: the size shall be greater than zero) and had the same cause --
/// a negative dimension also fell to `unwrap_or(0)` and was silently accepted.
#[test]
fn diagnostics_negative_array_size_is_rejected() {
    compile_expect_error("neg_array", "int bad[-1];\n", "negative");
    compile_expect_error(
        "neg_array_block",
        "int main(void){ int bad[-1]; return bad[0]; }\n",
        "negative",
    );
}

/// The forms that must keep working: a constant size, an incomplete array at
/// file scope (a tentative definition, and the `extern` form), a zero-length
/// array (a GNU extension gcc accepts), and a VLA where it is legal.
#[test]
fn diagnostics_ordinary_file_scope_arrays_are_accepted() {
    compile_expect_ok("fs_const", "int ok[3];\n");
    compile_expect_ok("fs_incomplete", "int ok[];\n");
    compile_expect_ok("fs_extern_incomplete", "extern int ok[];\n");
    compile_expect_ok("fs_zero_len", "int ok[0];\n");
    compile_expect_ok("fs_enum_size", "enum { N = 4 }; int ok[N];\n");
    compile_expect_ok("fs_sizeof_size", "int ok[sizeof(int)];\n");
    compile_expect_ok(
        "block_vla_still_ok",
        "int main(void){ int n = 4; int ok[n]; ok[0] = 1; return ok[0] - 1; }\n",
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
        "too few arguments to function 'g'",
    );
    compile_expect_error(
        "call_too_many",
        "int g(int);\nint main(void){return g(1,2,3);}\n",
        "too many arguments to function 'g'",
    );
    compile_expect_error(
        "call_variadic_short",
        "int g(int,int,...);\nint main(void){return g(1);}\n",
        "too few arguments to function 'g'",
    );
    // A callee that is not a name is not named.
    compile_expect_error(
        "call_through_expression",
        "int (*fp[1])(int);\nint main(void){return fp[0](1, 2);}\n",
        "error: too many arguments to function\n",
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
        "different type",
    );
}

/// C11 legalized redefining a typedef to denote the *same* type (6.7p3), which two
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

/// The specifier form is not the only way to reach C17 6.7.3p3. The bare
/// qualifier applied to a typedef that names an array or function type is the
/// other, and it only set a modifier bit -- so both of these were accepted.
/// gcc rejects them.
#[test]
fn diagnostics_atomic_qualifier_on_array_or_function_typedef_is_rejected() {
    compile_expect_error(
        "atomic_typedef_array",
        "typedef int A[4];\n_Atomic A x;\n",
        "'_Atomic' cannot be applied to an array type",
    );
    compile_expect_error(
        "atomic_typedef_function",
        "typedef int F(void);\n_Atomic F f;\n",
        "'_Atomic' cannot be applied to a function type",
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
    // A typedef naming an ordinary object type is fine.
    compile_expect_ok(
        "ok_atomic_typedef_struct",
        "typedef struct S{int a;} T;\n_Atomic T t;\nint main(void){return 0;}\n",
    );
    compile_expect_ok(
        "ok_atomic_typedef_int",
        "typedef int I;\n_Atomic I v;\nint main(void){return 0;}\n",
    );
}

/// C17 6.7.2.4p3 also bars `_Atomic` on a variably-modified type. c17 needs no
/// separate check for that, because such a struct or union cannot be formed in
/// the first place — both routes to one are already closed earlier in the
/// parse, and an unreachable constraint would be dead code.
///
/// Pinned here so the reasoning is checkable: if either gate below is ever
/// relaxed, `_Atomic` grows a real hole and these tests say where to look.
#[test]
fn diagnostics_variably_modified_aggregates_cannot_be_formed() {
    // Directly: C99 6.7.5.2 forbids a VLA member outright.
    compile_expect_error(
        "vla_member",
        "void f(int n){ struct S { int a[n]; } s; (void)s; }\n",
        "variable length arrays cannot be structure or union members",
    );
    // And through a typedef, which is the only way to smuggle the
    // variably-modified part past the member declarator (#L5).
    compile_expect_error(
        "vm_member_via_typedef",
        "void f(int n){ typedef int A[n]; struct S { A x; } s; (void)s; }\n",
        "a member of a structure or union cannot have a variably modified type",
    );
    // So the _Atomic spelling fails on the type, never reaching the qualifier.
    compile_expect_error(
        "atomic_vla_member",
        "void f(int n){ _Atomic struct S { int a[n]; } s; (void)s; }\n",
        "variable length arrays cannot be structure or union members",
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

// ============================================================================
// Regressions found in review: checks that fired on legal code
// ============================================================================

/// #L1's implicit-int check was once driven by a parser-wide flag that the
/// specifier parser set. Its struct/union/enum/typeof arms returned early
/// without setting it, so a specifier-less call — a K&R identifier list,
/// whose undeclared parameters have type `int` by C17 6.9.1p6 — left the flag
/// false and the *next* declaration inherited the complaint. The answer is
/// now part of what the specifier parser returns, so nothing can go stale.
#[test]
fn diagnostics_kr_parameter_list_does_not_poison_the_next_declaration() {
    compile_expect_ok(
        "kr_then_struct",
        "struct S { int a; };\n\
         int f(a) { return a; }\n\
         struct S s;\n\
         int main(void){ s.a = f(1); return s.a - 1; }\n",
    );
    // The same staleness via enum, union, and a typedef'd name.
    compile_expect_ok(
        "kr_then_enum",
        "enum E { E0 };\nint f(a) { return a; }\nenum E e;\nint g(void){ return (int)e; }\n",
    );
    compile_expect_ok(
        "kr_then_union",
        "union U { int a; };\nint f(a) { return a; }\nunion U u;\nint g(void){ return u.a; }\n",
    );
}

/// #X4 compares a typedef against whatever `lookup_id` finds, which is the
/// innermost *visible* binding in any enclosing scope. Shadowing a file-scope
/// typedef inside a block is legal C, not a redefinition.
#[test]
fn diagnostics_typedef_may_be_shadowed_in_an_inner_scope() {
    compile_expect_ok(
        "typedef_shadow_block",
        "typedef int T;\nint main(void){ typedef double T; T x = 1.5; return x == 1.5 ? 0 : 1; }\n",
    );
    // Nested blocks, and a shadow that reverts when the scope closes.
    compile_expect_ok(
        "typedef_shadow_nested",
        "typedef int T;\n\
         int main(void){\n\
           { typedef char T; T c = 'a'; if (sizeof(T) != 1) return 1; }\n\
           { typedef double T; if (sizeof(T) != 8) return 2; }\n\
           return sizeof(T) == sizeof(int) ? 0 : 3;\n\
         }\n",
    );
    // A function parameter may also shadow a file-scope typedef name.
    compile_expect_ok(
        "typedef_shadow_param",
        "typedef int T;\nint f(int T){ return T; }\nint main(void){ return f(0); }\n",
    );
    // ...but a genuine same-scope conflict must still be caught.
    compile_expect_error(
        "typedef_conflict_same_scope",
        "typedef int T;\ntypedef double T;\n",
        "different type",
    );
}

/// #L3 rejected every `return expr;` in a void function. C17 6.8.6.4p1 forbids
/// returning a *value*; an expression of type `void` has none, and
/// `return f();` where `f` returns void is the ordinary tail-call wrapper.
#[test]
fn diagnostics_return_of_a_void_expression_is_accepted() {
    compile_expect_ok(
        "return_void_call",
        "static int n;\nstatic void inner(void){ n = 1; }\n\
         static void outer(void){ return inner(); }\n\
         int main(void){ outer(); return n - 1; }\n",
    );
    // A cast to void, and a comma expression ending in one.
    compile_expect_ok(
        "return_void_cast",
        "static int n;\nvoid f(void){ return (void)n; }\n",
    );
    compile_expect_ok(
        "return_void_conditional",
        "static void a(void); static void b(void);\n\
         void f(int c){ return c ? a() : b(); }\n\
         static void a(void){} static void b(void){}\n",
    );
    // Returning an actual value from void is still an error.
    compile_expect_error(
        "return_int_from_void",
        "void f(void){ return 1; }\n",
        "'return' with a value in a function returning void",
    );
}

/// #L2's "not an integer constant expression" fires wherever `eval_const_expr`
/// returns nothing — but that evaluator is partial, so its gaps became compile
/// errors on valid labels. What it cannot fold it must not condemn.
#[test]
fn diagnostics_foldable_case_labels_are_accepted() {
    compile_expect_ok(
        "case_cast_from_double",
        "int f(int x){ switch(x){ case (int)2.0: return 13; default: return 0; } }\n",
    );
    compile_expect_ok(
        "case_cast_from_float_expr",
        "int f(int x){ switch(x){ case (int)(1.5 * 2.0): return 1;\n\
         case (int)'a': return 2; default: return 0; } }\n",
    );
    compile_expect_ok(
        "case_enum_and_arithmetic",
        "enum E { A = 3, B };\n\
         int f(int x){ switch(x){ case A: return 1; case B + 1: return 2;\n\
         case sizeof(int): return 3; default: return 0; } }\n",
    );
    // A label naming a runtime variable is still rejected.
    compile_expect_error(
        "case_runtime_value",
        "int f(int x, int y){switch(x){case y: return 1;} return 0;}\n",
        "not an integer constant expression",
    );
}

// ============================================================================
// #X3 — `_Generic` constraint violations (C17 6.5.1.1p2)
// ============================================================================

#[test]
fn diagnostics_generic_without_a_matching_association_is_rejected() {
    compile_expect_error(
        "generic_no_match",
        "int f(void){ char x=0; return _Generic(x, int:1, long:2); }\n",
        "not compatible with any association",
    );
}

#[test]
fn diagnostics_generic_duplicate_default_is_rejected() {
    compile_expect_error(
        "generic_two_defaults",
        "int f(void){ return _Generic(1, int:1, default:2, default:3); }\n",
        "more than one 'default'",
    );
}

#[test]
fn diagnostics_generic_compatible_associations_are_rejected() {
    compile_expect_error(
        "generic_dup_type",
        "int f(void){ return _Generic(1, int:1, int:2); }\n",
        "two associations with compatible type",
    );
    // A typedef names the same type, so it collides too. This only works
    // because a typedef's TypeId no longer carries the TYPEDEF bit.
    compile_expect_error(
        "generic_dup_typedef",
        "typedef int MyInt; int f(void){ return _Generic(1, int:1, MyInt:2); }\n",
        "two associations with compatible type",
    );
}

/// The negative tests above must not pass by rejecting every `_Generic`.
///
/// `int` and `const int` are *not* compatible (C17 6.7.3p10 requires
/// identically qualified versions), so both may appear -- even though the
/// `const int` arm can never be selected, since the controlling expression is
/// lvalue-converted to an unqualified type.
#[test]
fn diagnostics_generic_valid_forms_are_accepted() {
    for (name, src) in [
        (
            "generic_basic",
            "int f(void){ return _Generic(1, int:1, default:0); }\n",
        ),
        (
            "generic_default_only",
            "int f(void){ return _Generic((void*)0, default:7); }\n",
        ),
        (
            "generic_qualified_sibling",
            "int f(void){ return _Generic(1, int:1, const int:2); }\n",
        ),
        (
            "generic_no_default_but_matches",
            "int f(void){ return _Generic(1, int:1, long:2); }\n",
        ),
        (
            "generic_nested",
            "int f(void){ return _Generic(1.0, double: _Generic(1, int:5, default:0), default:0); }\n",
        ),
        (
            "generic_static_fn_call",
            "static int g(void){return 1;} int f(void){ return _Generic(g(), int:1, default:0); }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// ============================================================================
// Keywords are not declarator names
// ============================================================================

/// A keyword cannot name an object, and the statement keywords are the worst
/// of it: c17 accepted `int if;` and emitted a real symbol called `if`, into
/// `.data`, that no later C translation unit could ever refer to.
///
/// The declarator-name check only rejected identifiers tagged `TYPE_KEYWORD`,
/// which let through both the deliberately untagged `sizeof` family and every
/// statement keyword.
#[test]
fn diagnostics_keywords_are_rejected_as_declarator_names() {
    for kw in [
        // Deliberately untagged so they cannot be mistaken for the start of a
        // declaration; that is also what let them reach the name position.
        "sizeof",
        "_Generic",
        "_Alignof",
        "__alignof__",
        "__alignof",
        "_Static_assert",
        // A type specifier c17 provides no type for; in the name position it
        // is the name, not the specifier.
        "_Imaginary",
        // Statement keywords.
        "if",
        "else",
        "while",
        "do",
        "for",
        "return",
        "break",
        "continue",
        "goto",
        "switch",
        "case",
        "default",
    ] {
        compile_expect_error(
            &format!("kwname_{}", kw.trim_start_matches('_')),
            &format!("int {kw};\nint main(void){{return 0;}}\n"),
            "cannot be used as a name",
        );
    }
}

/// The same rule applies wherever a declarator name appears, not just at file
/// scope. A struct member called `if` was accepted too, and `p->if` parsed.
#[test]
fn diagnostics_keywords_are_rejected_in_every_declarator_position() {
    compile_expect_error(
        "kwname_member",
        "struct S { int if; };\nint main(void){return 0;}\n",
        "cannot be used as a name",
    );
    compile_expect_error(
        "kwname_param",
        "int f(int while);\nint main(void){return 0;}\n",
        "cannot be used as a name",
    );
    compile_expect_error(
        "kwname_local",
        "int main(void){ int return; return 0; }\n",
        "cannot be used as a name",
    );
    compile_expect_error(
        "kwname_func",
        "int sizeof(void) { return 0; }\nint main(void){return 0;}\n",
        "cannot be used as a name",
    );
}

/// Words that are *not* C17 keywords must stay usable as names, which is the
/// half of this that is easy to break. `alignof` and `typeof_unqual` are C23
/// spellings, `_BitInt` is C23, and the rest are ordinary identifiers that
/// merely appear in the keyword table for other purposes. gcc accepts every
/// one of these in C17 mode.
#[test]
fn diagnostics_non_keywords_remain_usable_as_names() {
    for name in [
        "typeof_unqual",
        "_BitInt",
        "L",
        "noreturn",
        "aligned",
        "packed",
        "restrict_",
    ] {
        compile_expect_ok(
            &format!("okname_{name}"),
            &format!("int {name};\nint main(void){{ {name} = 1; return {name} - 1; }}\n"),
        );
    }

    // None of these four is a C17 keyword either -- `offsetof` is a macro,
    // `alignof` a C23 spelling, `setjmp` and `longjmp` library functions -- so
    // each has to work in expression position too, not merely as a declarator
    // name. The parser used to recognise them ahead of ordinary lookup, so
    // `offsetof = 1;` reported "expected '('".
    for name in ["alignof", "offsetof", "setjmp", "longjmp"] {
        compile_expect_ok(
            &format!("okdecl_{name}"),
            &format!("int {name};\nint main(void){{ {name} = 1; return {name} - 1; }}\n"),
        );
    }
}

/// `offsetof`, `alignof`, `setjmp` and `longjmp` in every position a program
/// may put an identifier -- and still meaning the builtin where nothing has
/// claimed the name.
#[test]
fn diagnostics_shadowable_builtins_yield_to_a_declaration() {
    // A local, a parameter, a file-scope object taken by address, and a
    // function definition of the same name.
    compile_expect_ok(
        "shadow_local",
        "int main(void){ int offsetof = 2; int alignof = 3; return offsetof + alignof - 5; }\n",
    );
    compile_expect_ok(
        "shadow_param",
        "static int f(int alignof, int offsetof){ return alignof + offsetof; }\n\
         int main(void){ return f(2, -2); }\n",
    );
    compile_expect_ok(
        "shadow_addr",
        "int alignof;\nint main(void){ int *p = &alignof; *p = 0; return *p; }\n",
    );
    compile_expect_ok(
        "shadow_fn",
        "static int offsetof(int x){ return x; }\nint main(void){ return offsetof(0); }\n",
    );

    // `setjmp` yields to an object but not to a function declaration: that is
    // what <setjmp.h> provides, and it needs code generation an ordinary call
    // cannot produce.
    compile_expect_ok(
        "shadow_setjmp_object",
        "int main(void){ int setjmp = 0; setjmp = 1; return setjmp - 1; }\n",
    );
    compile_expect_ok(
        "shadow_setjmp_header",
        "#include <setjmp.h>\n\
         static jmp_buf env;\n\
         int main(void){ if (setjmp(env) != 0) return 0; longjmp(env, 1); return 1; }\n",
    );

    // Undeclared, the builtin meaning still applies.
    compile_expect_ok(
        "shadow_none",
        "#include <stddef.h>\n\
         struct S { int a; int b; };\n\
         int main(void){ return offsetof(struct S, b) == sizeof(int) ? 0 : 1; }\n",
    );
}

/// C17 6.7.2p2 admits only a fixed list of type-specifier combinations.
///
/// The specifier loop tracked just the resulting kind, each keyword
/// overwriting the last, so an impossible combination silently named whichever
/// type came last: `float double x;` was a `double`, `void int y;` an object of
/// type void with a size of 4, `long long long z;` a `long long`.
#[test]
fn diagnostics_conflicting_type_specifiers_are_rejected() {
    for (idx, decl) in [
        "int int x;",
        "int char x;",
        "float double x;",
        "void int x;",
        "int _Bool x;",
        "short long x;",
        "signed unsigned x;",
        "long float x;",
        "unsigned float x;",
        "signed void x;",
        "unsigned _Bool x;",
    ]
    .iter()
    .enumerate()
    {
        compile_expect_error(&format!("badspec_{idx}"), decl, "declaration specifiers");
    }

    compile_expect_error("badspec_toolong", "long long long x;", "too long");
    for (idx, decl) in ["short short x;", "signed signed x;", "unsigned unsigned x;"]
        .iter()
        .enumerate()
    {
        compile_expect_error(&format!("baddup_{idx}"), decl, "duplicate");
    }

    // Struct members and block scope go through the same path.
    compile_expect_error(
        "badspec_member",
        "struct S { int int x; };\n",
        "declaration specifiers",
    );
    compile_expect_error(
        "badspec_block",
        "int main(void){ int int y; return y; }\n",
        "declaration specifiers",
    );

    // Every combination C17 6.7.2p2 does admit must still compile, including
    // the ones that look like duplicates.
    for (idx, decl) in [
        "short int x;",
        "long int x;",
        "long long int x;",
        "long unsigned int x;",
        "signed long long x;",
        "short unsigned x;",
        "unsigned char x;",
        "signed char x;",
        "long double x;",
        "double _Complex x;",
        "long double _Complex x;",
        "unsigned __int128 x;",
        "const volatile int x;",
    ]
    .iter()
    .enumerate()
    {
        compile_expect_ok(&format!("okspec_{idx}"), decl);
    }

    // An alias spelling a C library may itself define as a typedef stays a
    // typedef: glibc's <bits/floatn-common.h> has `typedef float _Float32;`.
    compile_expect_ok(
        "okspec_alias_typedef",
        "typedef float _Float32;\ntypedef double _Float64;\n\
         int main(void){ _Float32 a = 1.0f; _Float64 b = 2.0; return (a + b) == 3.0 ? 0 : 1; }\n",
    );
}

/// A label and a struct tag live in their own namespaces, and the check must
/// not reach them -- `expect_identifier` has eighteen callers.
#[test]
fn diagnostics_labels_and_tags_are_unaffected() {
    compile_expect_ok(
        "okname_label",
        "int main(void){ int n = 0; done: if (n) goto done; return 0; }\n",
    );
    compile_expect_ok(
        "okname_tag",
        "struct offsetof { int x; };\nint main(void){ struct offsetof s; s.x = 0; return s.x; }\n",
    );
}

// ============================================================================
// Floating suffixes on integer constants
// ============================================================================

/// `q` and `f128` are *floating* suffixes, so neither attaches to an integer
/// constant.
///
/// Both were accepted at first, silently reinterpreting an integer as a
/// binary128: `return 1q;` compiled and returned garbage. gcc rejects both
/// with "invalid suffix on integer constant". The `f128` half survived the
/// first fix because only `q` was gated on the literal being floating.
#[test]
fn diagnostics_binary128_suffixes_need_a_floating_constant() {
    for (name, src) in [
        ("int_q", "int main(void){ return 1q; }\n"),
        ("int_f128", "int main(void){ return 1f128; }\n"),
        ("octal_q", "int main(void){ return 07q; }\n"),
    ] {
        compile_expect_error(name, src, "invalid integer literal");
    }

    // A hex integer whose last digits merely spell a suffix is still an
    // integer, on every target.
    compile_expect_ok(
        "hex_int_spelling_a_suffix",
        "int main(void){ return 0x1f128 != 127272; }\n",
    );

    // The floating forms are accepted where the type exists. Where it does
    // not, a `q` literal has nowhere to live and is rejected with it, so the
    // body is compiled out rather than asserted either way.
    compile_expect_ok(
        "binary128_literals",
        concat!(
            "#include <float.h>\n",
            "#ifdef __FLT128_MANT_DIG__\n",
            "__float128 a = 1.0q;\n",
            "__float128 b = 0x1p0f128;\n",
            "int main(void){ return a != b; }\n",
            "#else\n",
            "int main(void){ return 0; }\n",
            "#endif\n",
        ),
    );
}

// ==== #L6 residual — a zero-parameter prototype's call arity (C17 6.5.2.2p2) ====

/// `int f(void)` and `int f()` are different types, not the same type spelled
/// two ways: C17 6.7.6.3p14 makes an empty *identifier* list supply no
/// information about the parameters, while `(void)` says there are none. Both
/// interned as an empty parameter vector, so nothing downstream could tell
/// "unknown" from "none".
///
/// That cost a diagnostic in one direction and produced a wrong one in the
/// other. A call to `int f(void)` with arguments went unchecked, though
/// 6.5.2.2p2 makes it a constraint violation; and a call to a K&R definition
/// *was* checked, though 6.5.2.2p1 permits no check against a declarator with
/// no prototype -- so `int f(a,b) int a,b; {...}` called as `f(1)` was
/// rejected, where gcc accepts it.
#[test]
fn diagnostics_zero_parameter_prototype_arity_is_rejected() {
    compile_expect_error(
        "void_proto_too_many_args",
        "int f(void);\nint main(void){ return f(1, 2); }\nint f(void){ return 0; }\n",
        "too many arguments to function 'f'",
    );
    compile_expect_error(
        "void_definition_too_many_args",
        "int f(void){ return 0; }\nint main(void){ return f(1, 2); }\n",
        "too many arguments to function 'f'",
    );
    compile_expect_error(
        "void_proto_one_arg",
        "int f(void);\nint main(void){ return f(7); }\nint f(void){ return 0; }\n",
        "too many arguments to function 'f'",
    );
}

/// The other direction: a declarator with no prototype accepts any argument
/// list, and must not be checked.
#[test]
fn diagnostics_unprototyped_calls_are_accepted() {
    for (name, src) in [
        // An empty identifier list says nothing about the parameters.
        (
            "empty_list_decl",
            "int f();\nint main(void){ return f(1, 2); }\nint f(int a, int b){ return a + b; }\n",
        ),
        // A K&R definition is likewise unprototyped -- this used to be
        // rejected with "too few arguments to function 'f'".
        (
            "kr_definition_too_few",
            "int f(a, b) int a, b; { return a + b; }\nint main(void){ return f(1); }\n",
        ),
        (
            "kr_definition_too_many",
            "int f(a, b) int a, b; { return a + b; }\nint main(void){ return f(1, 2, 3); }\n",
        ),
        // And the correct calls against a real prototype still compile.
        (
            "void_proto_no_args",
            "int f(void);\nint main(void){ return f(); }\nint f(void){ return 0; }\n",
        ),
        (
            "proto_exact_args",
            "int f(int, int);\nint main(void){ return f(1, 2); }\nint f(int a, int b){ return a + b; }\n",
        ),
        (
            "variadic_extra_args",
            "#include <stdio.h>\nint main(void){ printf(\"%d %d\\n\", 1, 2); return 0; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// C17 6.5.3.3p1: the operand of unary `+` or `-` has arithmetic type. A
/// pointer, array or structure operand compiled in silence -- `+p` was `p`.
/// Worded as gcc words it.
#[test]
fn diagnostics_unary_plus_and_minus_need_arithmetic_operands() {
    for (name, src, expected) in [
        (
            "unary_plus_pointer",
            "int *p;\nint f(void){ (void)+p; return 0; }\n",
            "wrong type argument to unary plus",
        ),
        (
            "unary_plus_struct",
            "struct S { int x; } s;\nint f(void){ (void)+s; return 0; }\n",
            "wrong type argument to unary plus",
        ),
        (
            "unary_minus_pointer",
            "int *p;\nint f(void){ (void)-p; return 0; }\n",
            "wrong type argument to unary minus",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

// ==== lvalue constraints (C17 6.5.16p2, 6.5.3.1p1, 6.5.3.2p1) ====

/// Assignment and the increment operators require a *modifiable lvalue*, and
/// unary `&` an object that has an address. None of it was checked: `a+b = 3`,
/// `v = w` between arrays, `(a+1)++` and `&reg` all compiled silently, so a
/// program that could not mean anything was translated into one that did
/// something.
///
/// The messages deliberately match gcc's, since those are the words a user
/// searches for.
#[test]
fn diagnostics_non_lvalue_targets_are_rejected() {
    for (name, src, expected) in [
        (
            "assign_to_sum",
            "int main(void){ int a=1,b=2; a+b = 3; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        (
            "assign_to_cast",
            "int main(void){ int a=1; (int)a = 2; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        // Unary `+` yields a value (C17 6.5.3.3p2); it returned its operand,
        // lvalue and all.
        (
            "assign_to_unary_plus",
            "int main(void){ int a=1; +a = 2; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        (
            "address_of_unary_plus",
            "int main(void){ int a=1; int *p = &+a; return *p; }\n",
            "lvalue required as unary '&' operand",
        ),
        (
            "assign_to_call",
            "int f(void);\nint main(void){ f() = 1; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        (
            "assign_to_conditional",
            "int main(void){ int a=1,b=2; (1?a:b) = 3; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        (
            "assign_to_array",
            "int main(void){ int v[3],w[3]; v = w; return 0; }\n",
            "assignment to expression with array type",
        ),
        // A function designator is not an lvalue either. Both binders that
        // reach a non-defining declarator used `Symbol::variable`, and
        // `is_lvalue` asks the symbol's kind rather than its type -- so these
        // compiled and stored through the function's own address.
        (
            "assign_to_trailing_declarator_function",
            "int f(int), g(int);\nint main(void){ g = 0; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        (
            "assign_to_block_scope_function",
            "int main(void){ int g(int); g = 0; return 0; }\n",
            "lvalue required as left operand of assignment",
        ),
        (
            "preinc_non_lvalue",
            "int main(void){ int a=1; ++(a+1); return 0; }\n",
            "lvalue required as increment operand",
        ),
        (
            "postinc_non_lvalue",
            "int main(void){ int a=1; (a+1)++; return 0; }\n",
            "lvalue required as increment operand",
        ),
        (
            "postdec_non_lvalue",
            "int main(void){ int a=1; (a+1)--; return 0; }\n",
            "lvalue required as decrement operand",
        ),
        (
            "address_of_register",
            "int main(void){ register int a=1; return *&a; }\n",
            "address of register variable 'a' requested",
        ),
        // Unary `&` needs an lvalue or a function designator (6.5.3.2p1).
        // Anything else compiled, and took the address of a temporary.
        (
            "address_of_sum",
            "int main(void){ int a=1; int *p = &(a+1); return *p; }\n",
            "lvalue required as unary '&' operand",
        ),
        (
            "address_of_call",
            "int f(void);\nint main(void){ int *p = &f(); return *p; }\n",
            "lvalue required as unary '&' operand",
        ),
        (
            "address_of_conditional",
            "int main(void){ int a=1,b=2; int *p = &(a ? a : b); return *p; }\n",
            "lvalue required as unary '&' operand",
        ),
        (
            "address_of_member_of_call",
            "struct S { int x; };\nstruct S g(void);\n\
             int main(void){ int *p = &g().x; return *p; }\n",
            "lvalue required as unary '&' operand",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// The companion: every shape that *is* a modifiable lvalue must still assign,
/// step, and yield its address. A check that rejects `f().x` must not also
/// reject `s.x`, and one that rejects an array assignment must not reject an
/// assignment to its element.
#[test]
fn diagnostics_ordinary_lvalues_are_accepted() {
    let src = r#"
struct S { int x; int arr[3]; };
struct Outer { struct S s; };
union U { int i; float f; };
int garr[4];
struct S gs;
struct S *gp = &gs;

int main(void) {
    int a = 1, *pa = &a;
    struct S s = {0};
    struct Outer o = {{0}};
    union U u;
    int m[2][3];
    char buf[8];
    double _Complex z = 1.0;

    a = 2; a++; ++a; a--; --a; (void)&a;
    *pa = 3; (*pa)++; (void)&*pa;
    garr[1] = 4; garr[1]++; (void)&garr[1];
    s.x = 5; s.x++; (void)&s.x;
    s.arr[2] = 6; s.arr[2]++; (void)&s.arr[2];
    o.s.x = 7; (void)&o.s.x;
    gp->x = 8; gp->x++; (void)&gp->x;
    u.i = 9; (void)&u.i;
    m[1][2] = 10; m[1][2]++; (void)&m[1][2];
    buf[0] = 'x'; (void)&buf[0]; (void)&buf;
    /* gcc documents __real__/__imag__ as lvalues when the operand is one */
    __real__ z = 2.0; __imag__ z = 3.0;
    *(int *)buf = 11;
    (void)&"literal"[0];
    /* a compound literal is an object, so it is an lvalue */
    s = (struct S){1, {2,3,4}};
    (void)&(struct S){0};
    /* `&` also takes a function designator, and __func__ is an array */
    int (*fp)(void) = &main; (void)fp; (void)&*fp;
    (void)&__func__; (void)&__real__ z;
    return 0;
}
"#;
    compile_expect_ok("ordinary_lvalues", src);
}

// ==== assignment compatibility (C17 6.5.16.1, and 6.8.6.4p3 / 6.5.2.2p2) ====

/// Simple assignment, `return`, and argument passing share one set of
/// constraints: the standard defines the latter two as conversion "as if by
/// assignment". None of the three checked anything, so `int *p; p = 1.5;`
/// compiled to a `cvttsd2si` and left the pointer holding 1.
///
/// The severity split follows gcc exactly -- a conversion that does not exist
/// is an error, one that exists but is almost certainly a mistake is a warning
/// -- because that is what lets code which builds today keep building.
#[test]
fn diagnostics_incompatible_assignment_is_rejected() {
    for (name, src, expected) in [
        (
            "assign_ptr_from_double",
            "void f(void){ int *p; double d = 0; p = d; }\n",
            "incompatible types when assigning",
        ),
        (
            "assign_double_from_ptr",
            "void f(void){ double d; int *p = 0; d = p; }\n",
            "incompatible types when assigning",
        ),
        (
            "assign_struct_from_other_struct",
            "struct A{int x;}; struct B{int x;};\nvoid f(void){ struct A a; struct B b; a = b; }\n",
            "from type 'struct B'",
        ),
        (
            "assign_struct_from_int",
            "struct A{int x;};\nvoid f(void){ struct A a; int i = 0; a = i; }\n",
            "incompatible types when assigning",
        ),
        (
            "assign_from_void_call",
            "void v(void);\nvoid f(void){ int i; i = v(); }\n",
            "void value not ignored",
        ),
        (
            "return_ptr_from_double",
            "int *f(void){ return 1.5; }\n",
            "incompatible types when returning",
        ),
        (
            "return_struct_mismatch",
            "struct A{int x;}; struct B{int x;};\nstruct A f(void){ struct B b; return b; }\n",
            "incompatible types when returning",
        ),
        (
            "argument_ptr_from_double",
            "int g(int *);\nvoid f(void){ g(1.5); }\n",
            "incompatible type for argument 1",
        ),
        (
            "argument_struct_mismatch",
            "struct A{int x;}; struct B{int x;};\nint g(struct A);\nvoid f(void){ struct B b; g(b); }\n",
            "incompatible type for argument 1",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// Every conversion C17 6.5.16.1p1 permits must still compile -- and the
/// carve-outs are the ones that matter, because a check written from the types
/// alone would reject them. `p = 0` uses a null pointer constant, which is
/// spelled as an integer; `_Bool b = p` asks whether a pointer is null; and
/// `void *` converts both ways.
#[test]
fn diagnostics_permitted_assignments_are_accepted() {
    let src = r#"
struct A { int x; };
typedef int (*FP)(void);

int g(int *);
int h(int, double);
int fn(void);

int *ret_null(void) { return 0; }
void *ret_void_ptr(void) { int *p = 0; return p; }
const char *ret_lit(void) { return "hi"; }
FP ret_fn(void) { return fn; }
double ret_widened(void) { return 1; }

void f(void) {
    int i; double d; _Bool b;
    int *p; const int *cp; void *v; char buf[4];
    struct A a1, a2;

    i = d;  d = i;              /* arithmetic converts freely */
    p = 0;                      /* null pointer constant */
    p = (void *)0;
    b = p;                      /* 6.5.16.1p1: _Bool from a pointer */
    p = v;  v = p;              /* void * either way */
    cp = p;                     /* adding a qualifier is fine */
    p = buf;                    /* an array decays */
    a1 = a2;                    /* identical struct types */

    (void)g(0);
    (void)g(p);
    (void)h(1, 2.0);
    (void)i; (void)cp; (void)b;
}
"#;
    compile_expect_ok("permitted_assignments", src);
}

// ============================================================================
// #C56 — `void *` against a function pointer
// ============================================================================

/// 6.5.16.1p1 offers the `void *` carve-out for a pointer to an **object**
/// type, so a function pointer on the other side is a constraint violation.
/// It is a warning rather than a rejection: gcc accepts it in silence and
/// only `-pedantic` objects, and POSIX requires the line it appears in to
/// work -- `dlsym` returns `void *` and every caller assigns it to a function
/// pointer.
///
/// All four contexts, because 6.5.16.1's constraints reach `return` and
/// argument passing through "as if by assignment" and the three live in
/// different files.
#[test]
fn diagnostics_function_pointer_and_void_pointer_warn() {
    let cases = [
        (
            "fnptr_init",
            "typedef int (*FP)(void);\nFP f(void *v) { FP p = v; return p; }\n",
            "ISO C forbids initialization between function pointer and 'void *'",
        ),
        (
            "fnptr_assign",
            "int fn(void);\nvoid f(void **out) { *out = fn; }\n",
            "ISO C forbids assignment between function pointer and 'void *'",
        ),
        (
            "fnptr_return",
            "int fn(void);\nvoid *f(void) { return fn; }\n",
            "ISO C forbids return between function pointer and 'void *'",
        ),
        (
            "fnptr_argument",
            "int fn(void);\nvoid take(void *);\nvoid f(void) { take(fn); }\n",
            "ISO C forbids passing argument 1 between function pointer and 'void *'",
        ),
    ];
    for (name, src, expected) in cases {
        compile_expect_warning(name, src, expected);
    }
}

/// The warning must not reach an ordinary object pointer, and must not reach
/// a function designator converting to its own pointer type -- both are
/// conversions the standard permits outright, and a check written from
/// "pointer meets pointer" would catch them.
///
/// `compile_expect_ok` asserts only that the program builds, which a
/// spuriously warning compiler still does; this asserts the silence.
#[test]
fn diagnostics_function_pointer_warning_does_not_over_fire() {
    let src = r#"
typedef int (*FP)(void);
int fn(void);
FP ret_fn(void) { return fn; }
void f(void) {
    void *v; int *p; char *cp; _Bool b;
    p = v;  v = p;  cp = v;  v = cp;
    b = v;  v = 0;
    FP g = fn;  (void)g;  (void)b;
}
"#;
    let c = create_c_file("fnptr_no_over_fire", src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
    assert!(run.success, "should compile: {}", run.stderr);
    assert!(
        !run.stderr.contains("ISO C forbids"),
        "no permitted conversion may draw the #C56 warning, got:\n{}",
        run.stderr
    );
}

/// Diagnosing this at all is stricter than gcc's default, so it has to be
/// silenceable by name -- otherwise every `dlsym` caller pays for it.
#[test]
fn diagnostics_function_pointer_warning_can_be_silenced() {
    let src = "int fn(void);\nvoid *f(void) { return fn; }\n";
    let c = create_c_file("fnptr_silence", src);
    let path = c.path().to_string_lossy().to_string();

    for silencer in ["-w", "-Wno-function-pointer-conv"] {
        let run = run_c17(&["-S", "-o", "/dev/null", silencer, &path]);
        assert!(run.success, "{silencer} should be accepted: {}", run.stderr);
        assert!(
            !run.stderr.contains("ISO C forbids"),
            "{silencer} should silence the conversion warning, got:\n{}",
            run.stderr
        );
    }

    // An unrelated -Wno- must not silence it, or the flag name means nothing.
    let run = run_c17(&["-S", "-o", "/dev/null", "-Wno-unused", &path]);
    assert!(
        run.stderr.contains("ISO C forbids"),
        "-Wno-unused should leave it alone, got:\n{}",
        run.stderr
    );
}

/// glibc declares the socket calls with a union parameter carrying
/// `__attribute__((transparent_union))`, so a caller may hand them any one of
/// its member types -- `sendto(..., SAS2SA(&addr), ...)` is two lines of
/// CPython's socketmodule.c, and with them every socket program on the
/// platform.
///
/// The attribute is now recorded, so this is a rule about *transparent*
/// unions rather than about unions. The real header call and a synthetic twin
/// carrying the attribute are accepted; the ordinary union below is not.
#[test]
fn diagnostics_transparent_union_parameter_accepts_a_member_type() {
    compile_expect_ok(
        "transparent_union_socket_call",
        r#"
#include <sys/socket.h>
#include <netinet/in.h>
int f(int fd) {
    struct sockaddr_in a;
    socklen_t l = sizeof a;
    return getsockname(fd, (struct sockaddr *)&a, &l);
}
"#,
    );
    // The attribute on the union specifier...
    compile_expect_ok(
        "transparent_union_on_specifier",
        "union U { int *ip; char *cp; } __attribute__((transparent_union));
int g(union U);
void f(void){ int *p = 0; (void)g(p); }
",
    );
    // ...and glibc's own spelling, trailing on a typedef of an anonymous
    // union, in the underscored form its headers use.
    compile_expect_ok(
        "transparent_union_on_typedef",
        "typedef union { int *ip; char *cp; } UA __attribute__((__transparent_union__));
int g(UA);
void f(void){ int *p = 0; (void)g(p); }
",
    );
}

/// The accommodation that stood in for the attribute waved through *every*
/// union parameter, which under-diagnosed the ordinary case: 6.5.2.2p2 gives
/// an argument the constraints of simple assignment, and a member's type is
/// not the union's.
///
/// This is the half of the old `diagnostics_union_parameter_accepts_a_member_type`
/// whose premise inverted when the attribute became real.
#[test]
fn diagnostics_ordinary_union_parameter_rejects_a_member_type() {
    compile_expect_error(
        "ordinary_union_parameter_member_type",
        "union U { int *ip; char *cp; };
int g(union U);
void f(void){ int *p = 0; (void)g(p); }
",
        "incompatible type for argument 1",
    );
}

/// `transparent_union` is a union attribute. gcc ignores it elsewhere with a
/// warning rather than rejecting, and so must c17 -- silently dropping it
/// would leave the program believing a rule was in force that was not.
///
/// Every position that can carry it is covered, because they reach the check
/// by two different routes: the three specifier positions land on the
/// `CompositeType` as it is built, while the trailing-on-a-typedef spelling --
/// glibc's own -- is held over and applied once the declarator finishes. The
/// specifier ones were silently dropped when this landed; only the typedef
/// route warned.
#[test]
fn diagnostics_transparent_union_on_a_non_union_warns() {
    for (name, src) in [
        (
            "transparent_union_after_struct_body",
            "struct S { int a; } __attribute__((transparent_union));\nstruct S x;\n",
        ),
        (
            "transparent_union_before_struct_tag",
            "struct __attribute__((transparent_union)) T { int a; };\nstruct T y;\n",
        ),
        (
            "transparent_union_after_struct_tag",
            "struct U __attribute__((transparent_union)) { int a; };\nstruct U w;\n",
        ),
        (
            "transparent_union_on_struct_typedef",
            "typedef struct { int a; } SA __attribute__((transparent_union));\nSA z;\n",
        ),
    ] {
        compile_expect_warning(
            name,
            src,
            "'transparent_union' attribute ignored on a non-union type",
        );
    }
}

/// ...and it belongs to the `attributes` group, like every other
/// unimplemented-or-ignored attribute diagnostic.
#[test]
fn diagnostics_transparent_union_warning_can_be_silenced() {
    let src = "struct S { int a; } __attribute__((transparent_union));\nstruct S x;\n";
    let c = create_c_file("transparent_union_silence", src);
    let path = c.path().to_string_lossy().to_string();
    for silencer in ["-w", "-Wno-attributes"] {
        let run = run_c17(&["-S", "-o", "/dev/null", silencer, &path]);
        assert!(run.success, "{silencer} should be accepted: {}", run.stderr);
        assert!(
            !run.stderr.contains("transparent_union"),
            "{silencer} should silence it, got:\n{}",
            run.stderr
        );
    }
}

// ==== void operands and subscripts (C17 6.5.6p2, 6.5.15p3, 6.5.2.1p1) ====

/// An operand has to have a value. A call to a `void` function has none, so
/// `v() + 1` and `1 ? v() : 2` are constraint violations -- both used to
/// compile, the conditional taking whichever arm's type came first. And a
/// subscript needs a pointer on one side; `a[0]` where `a` is an `int` was
/// silently given the element type `int` and indexed anyway.
#[test]
fn diagnostics_void_operands_and_bad_subscripts_are_rejected() {
    for (name, src, expected) in [
        (
            "void_in_addition",
            "void v(void);\nint f(void){ return v() + 1; }\n",
            "void value not ignored",
        ),
        (
            "void_in_comparison",
            "void v(void);\nint f(void){ return v() == 0; }\n",
            "void value not ignored",
        ),
        (
            "void_in_conditional_then",
            "void v(void);\nint f(void){ int x = 1 ? v() : 2; return x; }\n",
            "void value not ignored",
        ),
        (
            "void_in_conditional_else",
            "void v(void);\nint f(void){ int x = 1 ? 2 : v(); return x; }\n",
            "void value not ignored",
        ),
        (
            "subscript_an_int",
            "int f(void){ int a = 1; return a[0]; }\n",
            "subscripted value is neither array nor pointer",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// The companion. A conditional whose arms are *both* void is fine, `void` is
/// allowed wherever its absence of value does not matter -- a cast, a comma's
/// left operand, a statement -- and a subscript stays symmetric: `a[i]` is
/// defined as `*(a + i)`, so `0[a]` is legal C.
#[test]
fn diagnostics_permitted_void_and_subscript_forms_are_accepted() {
    let src = r#"
void v(void);
int g(int *p) { return p[1]; }

int f(void) {
    int a[3] = {0};
    int *p = a;

    if (1) { v(); }             /* as a statement */
    (void)v();                  /* cast to void */
    (void)(v(), 1);             /* left operand of a comma */
    1 ? v() : v();              /* both arms void */

    return a[0] + 0[a] + p[2] + g(p);
}
"#;
    compile_expect_ok("permitted_void_and_subscripts", src);
}

// ==== declaration compatibility (C17 6.7p4, 6.2.7, 6.7.2.1p2, 6.7.6.3p10) ====

/// All declarations of one name in one scope must specify compatible types.
/// Nothing compared them: `SymbolTable::declare` rejects only two *definitions*
/// at one depth, and a function symbol is never marked defined, so two function
/// declarations never collided at all.
///
/// `int x; double x;` was therefore not merely undiagnosed -- it bound the
/// second declarator to the first symbol and emitted `.comm x,4,4`, so a
/// `double` store through it ran off the end of the object. That is the second
/// of the two silent miscompiles this series set out to close.
#[test]
fn diagnostics_conflicting_declarations_are_rejected() {
    for (name, src, expected) in [
        (
            "conflicting_object",
            "int x;\ndouble x;\n",
            "conflicting types for 'x'",
        ),
        (
            "conflicting_function",
            "int f(int);\nint f(char *);\n",
            "conflicting types for 'f'",
        ),
        (
            "conflicting_in_block",
            "int main(void){ int a; double a; return 0; }\n",
            "conflicting types for 'a'",
        ),
        (
            "function_then_object",
            "int f(void);\nint f;\n",
            "redeclared as a different kind of symbol",
        ),
        (
            "array_size_mismatch",
            "extern int a[3];\nint a[4];\n",
            "conflicting types for 'a'",
        ),
        (
            "duplicate_struct_member",
            "struct S { int a; int a; };\n",
            "duplicate member 'a'",
        ),
        (
            "duplicate_union_member",
            "union U { int a; float a; };\n",
            "duplicate member 'a'",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// The accept side, and it carries the weight here: a redeclaration check that
/// is even slightly too eager breaks every C program, because headers repeat
/// declarations constantly.
///
/// Each of these is a distinct reason the check must stay quiet -- a repeat, a
/// tentative definition, a prototype meeting its definition, 6.2.7p2's pairing
/// of an unprototyped declarator with a prototyped one, a storage class
/// changing between declarations, shadowing in an inner scope, a parameter
/// over a global, and 6.2.7p3's completion of an array type.
#[test]
fn diagnostics_compatible_redeclarations_are_accepted() {
    for (name, src) in [
        ("repeat_identical", "int x;\nint x;\nint main(void){ return x; }\n"),
        ("tentative_then_defined", "int x;\nint x = 3;\nint main(void){ return x - 3; }\n"),
        (
            "prototype_then_definition",
            "int f(int);\nint f(int a){ return a; }\nint main(void){ return f(0); }\n",
        ),
        // 6.2.7p2: no prototype, then one.
        ("unprototyped_then_prototyped", "int f();\nint f(int);\nint main(void){ return 0; }\n"),
        ("extern_then_definition", "extern int x;\nint x = 5;\nint main(void){ return x - 5; }\n"),
        (
            "static_then_definition",
            "static int f(void);\nstatic int f(void){ return 0; }\nint main(void){ return f(); }\n",
        ),
        // `inline` and `extern` ride on the *return* type, so a naive
        // comparison called these two `int(int)` different from each other.
        (
            "inline_then_extern",
            "inline int h(int a){ return a; }\nextern int h(int);\nint main(void){ return h(1) - 1; }\n",
        ),
        ("inner_scope_shadow", "int x;\nint main(void){ double x = 1; return (int)x - 1; }\n"),
        ("parameter_shadows_global", "int x;\nint f(double x){ return (int)x; }\nint main(void){ return f(0); }\n"),
        ("enum_constant", "enum E { A };\nint main(void){ return A; }\n"),
        // 6.2.7p3: an array of unknown size completed by a sized one.
        ("array_completion", "extern int a[];\nint a[3];\nint main(void){ return a[0]; }\n"),
        ("typedef_repeat", "typedef int T;\ntypedef int T;\nint main(void){ T x = 0; return x; }\n"),
        // Unnamed members all share the empty name and are not repeats.
        (
            "anonymous_and_unnamed_members",
            "struct S { int a; struct { int b; }; int :3; int :4; int c; };\nint main(void){ return 0; }\n",
        ),
        // 6.2.7p1: a tag names the type, so completing a forward declaration
        // does not create a second one. Comparing the two `CompositeType`
        // values structurally -- one incomplete and memberless -- called every
        // function declared before the definition and defined after it a
        // conflicting redeclaration. That is the shape of CPython's public
        // headers: `PyLongObject` is forward-declared, used in prototypes, and
        // completed later.
        (
            "forward_declared_struct_completed",
            "struct S;\nint f(const struct S *p);\nstruct S { int x; };\nint f(const struct S *p){ return p->x; }\nint main(void){ struct S s = {1}; return f(&s) - 1; }\n",
        ),
        (
            "forward_declared_struct_via_typedef",
            "typedef struct _o Obj;\nint g(const Obj *p);\nstruct _o { int x; };\nint g(const Obj *p){ return p->x; }\nint main(void){ struct _o o = {2}; return g(&o) - 2; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// ==== excess initializers (C17 6.7.9p2) ====

/// An initializer list may not hold more elements than the object it
/// initializes. gcc warns rather than failing, and so does c17 -- 5.1.1.3 asks
/// for a diagnostic, not a rejection.
#[test]
fn diagnostics_excess_initializers_are_diagnosed() {
    compile_expect_warning(
        "excess_scalar_initializer",
        "int main(void){ int a = {1, 2}; return a; }\n",
        "excess elements in scalar initializer",
    );
    compile_expect_warning(
        "excess_array_initializer",
        "int main(void){ int a[2] = {1, 2, 3}; return a[0]; }\n",
        "excess elements in array initializer",
    );
    compile_expect_warning(
        "excess_struct_initializer",
        "struct S { int a; };\nint main(void){ struct S s = {1, 2}; return s.a; }\n",
        "excess elements in struct initializer",
    );
    compile_expect_warning(
        "excess_global_array_initializer",
        "int g[2] = {1, 2, 3};\nint main(void){ return g[0]; }\n",
        "excess elements in array initializer",
    );
}

/// The counting is only unambiguous in the simple cases, and everything else
/// must stay silent -- a wrong warning here would fire on ordinary code.
///
/// Each of these is a distinct reason to say nothing: an exactly-filled or
/// short list, a bound taken from the initializer itself, a single braced
/// scalar, a designator that may place an element anywhere, brace elision
/// letting one aggregate member consume several elements, a union taking one
/// initializer whatever it holds, a flexible array member with no bound, and a
/// string literal initializing a character array in either spelling.
#[test]
fn diagnostics_well_sized_initializers_are_silent() {
    let src = r#"
struct P { int x, y; };
union U { int a; double b; };
struct F { int n; char d[]; };

int main(void) {
    int exact[3] = {1, 2, 3};
    int short_list[3] = {1};
    int inferred[] = {1, 2, 3};
    int braced_scalar = {1};
    int designated[3] = {[2] = 1};
    struct P elided[2] = {1, 2, 3, 4};
    union U u = {1};
    struct F f = {1};
    char s[4] = "ab";
    char b[4] = {"ab"};
    struct P nested = {1, 2};

    return exact[0] + short_list[0] + inferred[0] + braced_scalar
         + designated[2] + elided[1].y + u.a + f.n + s[0] + b[0] + nested.y;
}
"#;
    compile_expect_ok("well_sized_initializers", src);
}

// ==== regressions caught in review of this series ====

/// C17 6.7.6.1p2: two pointers are compatible only if they are *identically
/// qualified* and point at compatible types. Making compatibility recurse into
/// the referenced type (so that a storage class on an inner type could not
/// make one type look like two) briefly re-applied the ignore-top-level-
/// qualifiers rule at every level, which made `char *` and `const char *` the
/// same type.
///
/// The visible cost was not a missing diagnostic but valid code rejected:
/// `_Generic` saw two associations with "compatible" types and refused to
/// compile.
#[test]
fn diagnostics_pointer_target_qualifiers_are_part_of_the_type() {
    compile_expect_ok(
        "generic_distinguishes_qualified_pointers",
        "int f(char *x){ return _Generic((x), char *: 1, const char *: 2, default: 0); }\nint main(void){ char c = 0; return f(&c) - 1; }\n",
    );
    compile_expect_ok(
        "builtin_types_compatible_p_qualified_targets",
        "int main(void){\n  if (__builtin_types_compatible_p(char *, const char *)) return 1;\n  if (__builtin_types_compatible_p(int *, volatile int *)) return 2;\n  if (!__builtin_types_compatible_p(int, const int)) return 3;\n  if (!__builtin_types_compatible_p(int *, int *)) return 4;\n  return 0;\n}\n",
    );
    // A parameter is taken as having the unqualified version of its declared
    // type (6.7.6.3p15), so these two declarations are one type.
    compile_expect_ok(
        "parameter_qualifiers_do_not_split_a_prototype",
        "void f(const int);\nvoid f(int);\nint main(void){ return 0; }\n",
    );
    compile_expect_error(
        "qualified_return_conflicts",
        "char *f(void);\nconst char *f(void);\n",
        "conflicting types for 'f'",
    );
}

/// A GNU statement expression is an lvalue when the expression it ends with is
/// one. Omitting it from the lvalue predicate turned working code into a hard
/// error.
#[test]
fn diagnostics_statement_expressions_are_lvalues() {
    compile_expect_ok(
        "statement_expression_lvalue",
        "int main(void){ int x = 0; ({ x; }) = 5; ({ x; })++; return x - 6; }\n",
    );
}

/// `void` as the unnamed sole parameter means the function takes none, and
/// that is true however the type is spelled. Recognising only the literal
/// keyword made a typedef of `void` into a one-parameter prototype, which the
/// newly-enabled zero-arity check then turned into an error at every call.
#[test]
fn diagnostics_typedef_of_void_is_an_empty_parameter_list() {
    compile_expect_ok(
        "typedef_void_parameter",
        "typedef void V;\nint f(V);\nint f(void){ return 0; }\nint main(void){ return f(); }\n",
    );
}

/// The two directions of an integer/pointer mix read in opposite ways, and one
/// message for both names the wrong conversion half the time: assigning an
/// `int` to an `int *` "makes pointer from integer", not the reverse.
#[test]
fn diagnostics_integer_pointer_mix_names_its_direction() {
    compile_expect_warning(
        "pointer_from_integer",
        "void f(void){ int *p; int a = 0; p = a; }\n",
        "makes pointer from integer without a cast",
    );
    compile_expect_warning(
        "integer_from_pointer",
        "void f(void){ int a; int *p = 0; a = p; }\n",
        "makes integer from pointer without a cast",
    );
    // The same wording has to follow into the other two contexts.
    compile_expect_warning(
        "argument_pointer_from_integer",
        "int g(int *);\nvoid f(void){ (void)g(1); }\n",
        "makes pointer from integer without a cast",
    );
    compile_expect_warning(
        "return_pointer_from_integer",
        "int *f(void){ int a = 1; return a; }\n",
        "makes pointer from integer without a cast",
    );
}

/// C17 6.5.2.1p1 wants a pointer on one side of a subscript and an *integer*
/// on the other. Returning as soon as either side was a pointer accepted
/// `p[q]`, which has nothing to scale the offset by, and `p[1.5]`.
///
/// The two failures get different messages, as gcc gives them: the operand
/// that is present but wrong is a different mistake from neither being a
/// pointer at all.
#[test]
fn diagnostics_subscript_needs_a_pointer_and_an_integer() {
    compile_expect_error(
        "subscript_two_pointers",
        "void f(int *p, int *q){ (void)p[q]; }\n",
        "array subscript is not an integer",
    );
    compile_expect_error(
        "subscript_floating_index",
        "void f(int *p, double d){ (void)p[d]; }\n",
        "array subscript is not an integer",
    );
    compile_expect_error(
        "subscript_no_pointer",
        "int f(void){ int a = 1; return a[0]; }\n",
        "subscripted value is neither array nor pointer",
    );
}

/// Every integer type is a valid subscript, and the operands stay
/// interchangeable.
#[test]
fn diagnostics_integer_subscripts_are_accepted() {
    compile_expect_ok(
        "integer_subscripts",
        "enum E { A };\nint f(int *p, int i, char c, _Bool b, unsigned long u){\n  int a[3] = {0};\n  int m[2][3] = {{0}};\n  return a[i] + 0[a] + p[c] + p[b] + p[A] + p[u] + m[1][2];\n}\n",
    );
}

/// A tag names a type, but only within its scope: a nested `struct S` is a
/// different type from the outer one. Comparing tagged composites by tag alone
/// -- which is right while one side is still incomplete -- made them the same,
/// and the assignment check then accepted a copy of the wrong size.
#[test]
fn diagnostics_same_tag_in_another_scope_is_another_type() {
    compile_expect_error(
        "sibling_scope_struct_assignment",
        "struct S { int a; };\nvoid f(void){ struct S o; { struct S { double d; } in; in.d = 1.5; o = *(struct S *)&in; } (void)o; }\n",
        "incompatible types when assigning",
    );
}

// ==== unimplemented attributes (GCC extension) ====

/// An attribute the compiler does not implement used to be dropped in total
/// silence. That is survivable for one that only hints, and is not for one
/// that changes what the type *is*.
///
/// `vector_size` used to be rejected outright on the reasoning that no C
/// system header uses it. glibc's `<link.h>` does -- `La_x86_64_xmm` and its
/// siblings -- so the rejection made that header uncompilable. It is
/// implemented as storage now (see `c99_vector_size_has_a_vector_s_storage`);
/// what remains here is the warning for everything else unrecognised.
#[test]
fn diagnostics_unimplemented_attributes_are_reported() {
    compile_expect_warning(
        "unknown_attribute_warns",
        "typedef int T __attribute__((totally_made_up));\nint main(void){ return 0; }\n",
        "attribute directive ignored",
    );
    // A *vector* mode still needs vector types, so it keeps the warning; the
    // scalar modes are implemented (#C85) and must not warn.
    compile_expect_warning(
        "vector_mode_warns",
        "typedef float V __attribute__((__mode__(V4SF)));\nint main(void){ return 0; }\n",
        "'mode(V4SF)' is not implemented",
    );
}

/// `__attribute__((mode(M)))` replaces the declared type with the one of that
/// width in the same family, keeping the declared signedness (#C85).
///
/// Leaving it unimplemented was not the cosmetic problem the warning implied:
/// glibc declares `register_t` with `__mode__(__word__)`, so c17 sized it 4
/// bytes where gcc sizes it 8. The widths are checked by
/// `c99_mode_attribute_selects_the_type`; this pins that the ones c17 now
/// implements are silent, since 567 warnings per CPython build was the other
/// half of the complaint.
#[test]
fn diagnostics_implemented_modes_are_silent() {
    compile_expect_ok(
        "modes_silent",
        r#"
typedef int qi __attribute__((__mode__(__QI__)));
typedef int hi __attribute__((__mode__(__HI__)));
typedef int si __attribute__((__mode__(__SI__)));
typedef int di __attribute__((__mode__(__DI__)));
typedef int ti __attribute__((__mode__(__TI__)));
typedef int wd __attribute__((__mode__(__word__)));
typedef int pt __attribute__((__mode__(__pointer__)));
typedef float hf __attribute__((__mode__(__HF__)));
typedef float sf __attribute__((__mode__(__SF__)));
typedef float df __attribute__((__mode__(__DF__)));
typedef float xf __attribute__((__mode__(__XF__)));
typedef float tf __attribute__((__mode__(__TF__)));
typedef _Complex float hc __attribute__((__mode__(HC)));
typedef _Complex float sc __attribute__((__mode__(SC)));
typedef _Complex float dc __attribute__((__mode__(DC)));
typedef _Complex float xc __attribute__((__mode__(XC)));
typedef _Complex float tc __attribute__((__mode__(TC)));
int main(void){ return 0; }
"#,
    );
}

/// The attributes the compiler honours, and the ones it deliberately accepts
/// and ignores, must stay quiet — glibc's headers put them on nearly every
/// declaration, and a warning apiece would bury everything else.
///
/// `__has_attribute` has to agree with this set rather than keep a second list
/// of its own: it used to answer 0 for `ms_abi` and `gnu_inline`, which the
/// compiler implements, and 1 for four it silently ignored.
#[test]
fn diagnostics_recognised_attributes_are_silent() {
    let src = r#"
__attribute__((noreturn)) void die(void);
__attribute__((__const__)) int pure_fn(int);
__attribute__((nonnull(1))) int takes_ptr(void *);
__attribute__((__nothrow__)) int nothrows(void);
__attribute__((warn_unused_result)) int checked(void);
__attribute__((__returns_nonnull__)) void *never_null(void);
__attribute__((__leaf__, __artificial__)) int leafy(void);
struct __attribute__((packed)) P { char a; int b; };
__attribute__((aligned(16))) int aligned_var;
__attribute__((visibility("hidden"))) int hidden_var;
__attribute__((section(".mine"))) int placed_var;
__attribute__((weak)) int weak_var;
__attribute__((used)) static int used_var;

/* Checked at compile time, so the assertion cannot be skipped by a test
   helper that only builds and never runs. */
#if !__has_attribute(gnu_inline)
#error "__has_attribute must admit the attributes the compiler honours"
#endif
/* ms_abi is an x86-64 calling convention: honoured there, and on any other
   target ignored with a warning and so not claimed, as in gcc. */
#if defined(__x86_64__) != __has_attribute(ms_abi)
#error "__has_attribute(ms_abi) must answer whether the target honours it"
#endif
#if !__has_attribute(vector_size) || !__has_attribute(__mode__)
#error "__has_attribute must admit the type attributes the compiler implements"
#endif
#if !__has_attribute(weak) || !__has_attribute(transparent_union)
#error "__has_attribute must admit the attributes the compiler accepts"
#endif

int main(void) { return 0; }
"#;
    compile_expect_ok("recognised_attributes", src);
}

// ============================================================================
// Jumping into the scope of a variably modified type (C17 6.8.6.1p1)
// ============================================================================

/// Entering the scope of a variably modified identifier without executing its
/// declaration leaves the object's size never computed -- the array is
/// whatever the stack held -- so 6.8.6.1p1 forbids the jump outright. c17
/// accepted every form of it.
///
/// The diagnostic names the declaration that would have been skipped, where
/// gcc names only the kind of type; `Stmt::Goto` carries no position, and the
/// declaration is the more useful thing to point at anyway.
#[test]
fn diagnostics_jump_into_variably_modified_scope_is_rejected() {
    compile_expect_error(
        "goto_into_vla",
        "int main(void){int n=4; goto L; { int a[n]; L: return a[0]; } }\n",
        "jump into the scope of 'a'",
    );
    // The scope runs to the end of the block, so a label after the
    // declaration is inside it even without braces of its own.
    compile_expect_error(
        "goto_past_vla",
        "int main(void){int n=4; goto L; int a[n]; L: return a[0]; }\n",
        "variably modified type",
    );
    // A pointer to a variably modified array is variably modified too.
    compile_expect_error(
        "goto_into_ptr_to_vla",
        "int main(void){int n=4; goto L; { int (*p)[n]; L: return p != 0; } }\n",
        "jump into the scope of 'p'",
    );
    // A declaration in a for-init scopes over the body.
    compile_expect_error(
        "goto_into_for_init_vla",
        "int main(void){int n=4; goto L; for(int a[n];;){ L: return 0; } }\n",
        "variably modified type",
    );
    // Reaching a `case` transfers control from the `switch`, so the same rule
    // applies -- and says so.
    compile_expect_error(
        "switch_into_vla",
        "int main(void){int n=4,k=1; switch(k){ int a[n]; case 1: return a[0]; } return 0; }\n",
        "switch jump into the scope of 'a'",
    );
}

/// The jumps that stay legal. Without these the check could pass by rejecting
/// every `goto` near a VLA, which is the failure mode the whole diagnostics
/// suite exists to prevent.
#[test]
fn diagnostics_legal_jumps_around_variably_modified_scopes_are_accepted() {
    // Out of the scope, not into it.
    compile_expect_ok(
        "goto_out_of_vla",
        "int main(void){int n=4; L: ; { int a[n]; if(a[0]) goto L; } return 0; }\n",
    );
    // Within one scope.
    compile_expect_ok(
        "goto_within_vla",
        "int main(void){int n=4; { int a[n]; L: a[0]=1; if(a[0]) goto L; } return 0; }\n",
    );
    // To a label that precedes the declaration.
    compile_expect_ok(
        "goto_before_vla",
        "int main(void){int n=4; goto L; L: ; { int a[n]; return a[0]; } }\n",
    );
    // An ordinary array is not variably modified.
    compile_expect_ok(
        "goto_into_plain_array",
        "int main(void){goto L; { int a[4]; L: return a[0]; } }\n",
    );
    // The VLA is inside the case, not around it.
    compile_expect_ok(
        "switch_case_holds_vla",
        "int main(void){int n=4,k=1; switch(k){ case 1: { int a[n]; return a[0]; } } return 0; }\n",
    );
}

// ============================================================================
// Jumping into a GNU statement expression
// ============================================================================

/// The `error:` lines c17 prints for `src`, which must fail to compile.
fn compile_errors(name: &str, src: &str) -> Vec<String> {
    let c_file = create_c_file(name, src);
    let path = c_file.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
    assert!(!run.success, "{name}: expected a compile error");
    run.stderr
        .lines()
        .filter(|l| l.contains("error:"))
        .map(str::to_string)
        .collect()
}

/// gcc forbids entering a statement expression by `goto`, by `asm goto` or by
/// a `switch` reaching a `case` or `default` inside one: control would arrive
/// in the middle of evaluating the expression around it. c17 accepted every
/// form and generated the jump. Each is one error, in gcc's words, as many
/// times as gcc gives it.
#[test]
fn diagnostics_jump_into_statement_expression_is_rejected() {
    for (name, src, count) in [
        ("se_goto", "int f(int x) { goto L; return ({ L: x; }); }", 1),
        (
            "se_goto_sibling",
            "int f(void) { int a = ({ goto N; 1; }); int b = ({ N: 2; }); return a + b; }",
            1,
        ),
        (
            "se_goto_nested",
            "int f(void) { return ({ goto L; ({ L: 1; }); }); }",
            1,
        ),
        (
            "se_goto_inner_block",
            "int f(int x) { goto L; return ({ { L: x; } 1; }); }",
            1,
        ),
        (
            "se_goto_twice",
            "int f(int x) { goto L; ({ L: x; }); ({ M: x; }); goto M; return 0; }",
            2,
        ),
        (
            "se_asm_goto",
            "int f(int x) { asm goto (\"\" :::: R); return ({ R: x; }); }",
            1,
        ),
        // A label in an operand that is never evaluated is still written, so
        // the jump is the one error, as gcc has it: lowering never placed the
        // label and added "label 'L' used but not defined".
        (
            "se_goto_into_sizeof",
            "int f(int x) { goto L; return sizeof(({ L: x; })); }",
            1,
        ),
        (
            "se_goto_into_alignof",
            "int f(int x) { goto L; return _Alignof(({ L: x; })); }",
            1,
        ),
    ] {
        let errors = compile_errors(name, &format!("{src}\n"));
        assert_eq!(errors.len(), count, "{name}: {errors:?}");
        assert!(
            errors
                .iter()
                .all(|e| e.ends_with("error: jump into statement expression")),
            "{name}: {errors:?}"
        );
    }
}

#[test]
fn diagnostics_switch_into_statement_expression_is_rejected() {
    for (name, src, count) in [
        (
            "se_case",
            "int f(int x) { switch (x) { case 0: return ({ case 1: x; }); } return 0; }",
            1,
        ),
        (
            "se_default",
            "int f(int x) { switch (x) { case 0: return ({ default: x; }); } return 0; }",
            1,
        ),
        // At the top level of the switch body.
        (
            "se_case_top",
            "int f(int x) { switch (x) { ({ case 1: x++; }); } return x; }",
            1,
        ),
        // In a condition, and in an initializer inside a block.
        (
            "se_case_in_if",
            "int f(int x) { switch (x) { case 1: if (({ default: x; })) return 1; } return x; }",
            1,
        ),
        (
            "se_case_in_init",
            "int f(int x) { switch (x) { case 1: { int y = ({ case 3: x; }); return y; } } return x; }",
            1,
        ),
        // Past a switch nested inside the statement expression.
        (
            "se_case_past_inner_switch",
            "int f(int x) { switch (x) { case 0: x = ({ switch (x) { case 1: x; } case 2: 5; }); } return x; }",
            1,
        ),
        // Once per label.
        (
            "se_two_cases",
            "int f(int x) { switch (x) { case 0: x = ({ case 1: x; case 2: x; }); } return x; }",
            2,
        ),
    ] {
        let errors = compile_errors(name, &format!("{src}\n"));
        assert_eq!(errors.len(), count, "{name}: {errors:?}");
        assert!(
            errors
                .iter()
                .all(|e| e.ends_with("error: switch jumps into statement expression")),
            "{name}: {errors:?}"
        );
    }
}

/// Now that the check sees inside statement expressions, the rules it already
/// enforced reach there too: a label name is unique in its function, and a
/// loop's controlling expressions are not inside the loop.
#[test]
fn diagnostics_jump_rules_reach_inside_statement_expressions() {
    compile_expect_error(
        "se_duplicate_label",
        "int f(int x) { ({ L: x; }); ({ L: x; }); return 0; }\n",
        "duplicate label 'L'",
    );
    compile_expect_error(
        "se_break_in_while_cond",
        "int f(int x) { while (({ if (x) break; 1; })) x--; return x; }\n",
        "break statement not within loop or switch",
    );
    compile_expect_error(
        "se_continue_in_do_cond",
        "int f(int x) { do x--; while (({ if (x) continue; 1; })); return x; }\n",
        "continue statement not within a loop",
    );
    compile_expect_error(
        "se_break_in_for_step",
        "int f(int x) { for (;; ({ if (x) break; 1; })) x--; return x; }\n",
        "break statement not within loop or switch",
    );
    compile_expect_error(
        "se_case_in_switch_expr",
        "int f(int x) { switch (({ case 1: x; })) { case 2: ; } return x; }\n",
        "case label not within a switch statement",
    );
    // A variably modified scope inside a statement expression is both.
    let errors = compile_errors(
        "se_vla",
        "int f(int n) { goto L; ({ int a[n]; L: a[0]; }); return 0; }\n",
    );
    assert_eq!(errors.len(), 2, "{errors:?}");
    assert!(
        errors[0].contains("jump into the scope of 'a'"),
        "{errors:?}"
    );
    assert!(
        errors[1].ends_with("jump into statement expression"),
        "{errors:?}"
    );
}

/// Leaving a statement expression is allowed, as are jumps and switches wholly
/// inside one and a computed `goto`, which gcc leaves undiagnosed. These run,
/// so the jumps are shown to land where they should.
#[test]
fn diagnostics_legal_jumps_around_statement_expressions_are_accepted() {
    let src = r#"
int out(int x) { int y = ({ if (x) goto bail; x + 1; }); return y; bail: return -1; }
int within(void) { return ({ int r = 1; goto M; r = 5; M: r; }); }
int nested_out(int x) { return ({ int r = ({ if (x) goto P; 10; }); P: r; }); }
int back(int x) { L: x = ({ if (x > 3) goto L2; x + 1; }); if (x < 3) goto L; L2: return x; }
int sw(int x) { return ({ int r = 0; switch (x) { case 1: r = 7; break; default: r = 9; } r; }); }
int brk(int x) { for (;;) { x = ({ if (x > 5) break; x + 2; }); } return x; }
int main(void) {
    if (out(0) != 1 || out(2) != -1) return 1;
    if (within() != 1) return 2;
    if (nested_out(0) != 10) return 3;
    if (back(0) != 3) return 4;
    if (sw(1) != 7 || sw(4) != 9) return 5;
    if (brk(0) != 6) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("se_legal_jumps", src, &[]), 0);
    // gcc documents a computed `goto` into a statement expression as
    // undefined rather than diagnosing it, so it compiles; running it would
    // prove nothing.
    compile_expect_ok(
        "se_computed_goto",
        "int f(int x) { void *p = &&Q; goto *p; return ({ Q: x; }); }\n",
    );
    // The address of a label that is never evaluated still names a block --
    // one nothing reaches -- rather than a symbol no block defines.
    let src = "int f(int x) { void *p = &&U; return (int)sizeof(({ U: x; })) + (p != 0); }\n\
               int main(void) { return f(1) != 5; }\n";
    assert_eq!(compile_and_run("se_unevaluated_label_address", src, &[]), 0);
}

// ============================================================================
// C17 7.12.14 — the comparison macros take real floating arguments
// ============================================================================

/// C17 7.12.14p1 requires real floating arguments. gcc relaxes that to "both
/// real, at least one floating", and rejects the rest -- two integers, a
/// pointer, a complex value, a structure -- with this message. c17 accepted
/// two integers, citing gcc wrongly, and anything else was converted blindly.
#[test]
fn diagnostics_fp_compare_needs_a_floating_argument() {
    let builtins = [
        "__builtin_isgreater",
        "__builtin_isgreaterequal",
        "__builtin_isless",
        "__builtin_islessequal",
        "__builtin_islessgreater",
        "__builtin_isunordered",
        "__builtin_iseqsig",
    ];
    let bad = [
        ("int", "int"),
        ("char", "long"),
        ("_Bool", "_Bool"),
        ("enum E", "enum E"),
        ("int *", "double"),
        ("double", "void *"),
        ("_Complex double", "double"),
        ("_Complex int", "double"),
        ("struct S", "double"),
    ];
    for (i, b) in builtins.iter().enumerate() {
        for (j, (l, r)) in bad.iter().enumerate() {
            compile_expect_error(
                &format!("fpcmp_bad_{i}_{j}"),
                &format!("enum E {{ X }}; struct S {{ int a; }};\nint f({l} a, {r} b) {{ return {b}(a, b); }}\n"),
                &format!("non-floating-point arguments in call to function '{b}'"),
            );
        }
    }
}

#[test]
fn diagnostics_fp_compare_mixed_real_arguments_are_accepted() {
    let ok = [
        ("double", "double"),
        ("float", "long double"),
        ("int", "double"),
        ("double", "int"),
        ("float", "long"),
        ("_Bool", "double"),
        ("enum E", "float"),
        ("__int128", "double"),
    ];
    for (j, (l, r)) in ok.iter().enumerate() {
        compile_expect_ok(
            &format!("fpcmp_ok_{j}"),
            &format!(
                "enum E {{ X }};\nint f({l} a, {r} b) {{ return __builtin_isgreater(a, b) + __builtin_isunordered(b, a); }}\n"
            ),
        );
    }
    // Through <math.h>, whose macros expand to the builtins.
    compile_expect_ok(
        "fpcmp_math_h",
        "#include <math.h>\nint f(double d, float g, int i) { return isgreater(d, g) + isless(i, d) + isunordered(g, 1); }\n",
    );
}

// ============================================================================
// #C53 — a trailing comma in a parameter list (C17 6.7.6.3)
// ============================================================================

/// A parameter-type-list is a comma-separated list of parameter declarations,
/// optionally followed by `, ...`; nothing else may follow a comma. c17 let
/// the specifier parser supply an implicit `int` for the empty slot, so
/// `void g(int, );` silently declared `void(int, int)` -- and once call arity
/// was checked, the *correct* call `g(1)` became the one rejected. C23 allows
/// the trailing comma; this compiler is C17, and so is gcc here.
#[test]
fn diagnostics_trailing_comma_in_parameter_list_is_rejected() {
    for (name, src) in [
        ("tc_proto", "void g(int, );\nint main(void){return 0;}\n"),
        (
            "tc_defn",
            "void g(int a, ){(void)a;}\nint main(void){return 0;}\n",
        ),
        (
            "tc_two",
            "void g(int, char, );\nint main(void){return 0;}\n",
        ),
        (
            "tc_fnptr",
            "void g(int (*f)(int, ));\nint main(void){return 0;}\n",
        ),
    ] {
        compile_expect_error(name, src, "after ','");
    }
}

/// The list forms that must keep working -- including the call the old
/// behaviour turned into an error.
#[test]
fn diagnostics_ordinary_parameter_lists_are_accepted() {
    compile_expect_ok(
        "pl_correct_call",
        "void g(int);\nvoid g(int a){(void)a;}\nint main(void){ g(1); return 0; }\n",
    );
    compile_expect_ok(
        "pl_variadic",
        "int f(int, ...);\nint main(void){return 0;}\n",
    );
    compile_expect_ok("pl_void", "int f(void);\nint main(void){return 0;}\n");
    compile_expect_ok("pl_two", "int f(int, char);\nint main(void){return 0;}\n");
    compile_expect_ok("pl_empty", "int f();\nint main(void){return 0;}\n");
    compile_expect_ok(
        "pl_knr",
        "int f(a, b) int a, b; { return a+b; }\nint main(void){ return f(1,2)-3; }\n",
    );
}

// ============================================================================
// Universal character names naming forbidden characters (C17 6.4.3p2)
// ============================================================================

/// A UCN "shall not specify a character whose short identifier is less than
/// 00A0 other than 0024 ($), 0040 (@), or 0060 (`), nor one in the range D800
/// through DFFF inclusive."
///
/// Every one was accepted. The surrogate half was the worse of the two: a
/// surrogate has no `char`, so `char::from_u32` failed and both decoders took
/// that for "not an escape" and carried on with the letter `u`.
#[test]
fn diagnostics_forbidden_universal_character_names_are_rejected() {
    for (name, src) in [
        // In an identifier, at the start and in the middle: the lexer has a
        // separate decoder for each.
        (
            "ucn_ident_start",
            "int \\u0061bc = 3;\nint main(void){return 0;}\n",
        ),
        (
            "ucn_ident_mid",
            "int a\\u0062c = 3;\nint main(void){return 0;}\n",
        ),
        // In a string and in a character constant.
        (
            "ucn_string",
            "int main(void){ const char *s = \"\\u0041\"; return s[0]-65; }\n",
        ),
        (
            "ucn_charconst",
            "int main(void){ return (int)(char)'\\u0041' - 65; }\n",
        ),
        // A control character, and both ends of the surrogate range.
        (
            "ucn_space",
            "int main(void){ const char *s = \"\\u0020\"; return s[0]-32; }\n",
        ),
        (
            "ucn_surrogate_lo",
            "int main(void){ const char *s = \"\\ud800\"; return s[0]; }\n",
        ),
        (
            "ucn_surrogate_hi",
            "int main(void){ const char *s = \"\\udfff\"; return s[0]; }\n",
        ),
        // The long form is subject to the same rule.
        (
            "ucn_long_form",
            "int main(void){ const char *s = \"\\U00000041\"; return s[0]-65; }\n",
        ),
    ] {
        compile_expect_error(name, src, "not a valid universal character");
    }
}

/// The three carve-outs 6.4.3p2 names, and ordinary UCNs above 00A0.
#[test]
fn diagnostics_permitted_universal_character_names_are_accepted() {
    for (name, src) in [
        (
            "ucn_dollar",
            "int main(void){ const char *s = \"\\u0024\"; return s[0]-36; }\n",
        ),
        (
            "ucn_at",
            "int main(void){ const char *s = \"\\u0040\"; return s[0]-64; }\n",
        ),
        (
            "ucn_backtick",
            "int main(void){ const char *s = \"\\u0060\"; return s[0]-96; }\n",
        ),
        (
            "ucn_latin",
            "int main(void){ const char *s = \"\\u00e9\"; return s[0]!=0?0:1; }\n",
        ),
        (
            "ucn_ident_ok",
            "int \\u00c5ngstrom = 7;\nint main(void){ return 0; }\n",
        ),
        (
            "ucn_astral",
            "int main(void){ const char *s = \"\\U0001F600\"; return s[0]!=0?0:1; }\n",
        ),
        (
            "ucn_wide_char",
            "int main(void){ return (int)L'\\u00e9' - 233; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// ============================================================================
// #C90 — sizeof of an incomplete type (C17 6.5.3.4p1)
// ============================================================================

/// `sizeof` shall not be applied to an incomplete type. An array is the
/// awkward case: the type table cannot tell `int[n]` from `int[]`, since both
/// simply have no extent. The size expressions decide it -- a level with an
/// expression is variably modified and so complete, a level without one is
/// incomplete -- which is what makes `sizeof(int[][n])`, two absent extents
/// against one expression, the incomplete array of arrays gcc rejects.
#[test]
fn diagnostics_sizeof_of_an_incomplete_type_is_rejected() {
    for (name, src) in [
        (
            "sz_arr_of_vla",
            "int main(void){ int n = 4; return (int)sizeof(int[][n]); }\n",
        ),
        (
            "sz_incomplete_arr",
            "int main(void){ return (int)sizeof(int[]); }\n",
        ),
        (
            "sz_arr_of_arr",
            "int main(void){ return (int)sizeof(int[][3]); }\n",
        ),
        (
            "sz_undef_struct",
            "struct U;\nint main(void){ return (int)sizeof(struct U); }\n",
        ),
        (
            "sz_undef_union",
            "union U;\nint main(void){ return (int)sizeof(union U); }\n",
        ),
        (
            "sz_undef_enum",
            "enum E;\nint main(void){ return (int)sizeof(enum E); }\n",
        ),
    ] {
        compile_expect_error(name, src, "incomplete type");
    }
}

/// Everything `sizeof` must still accept, including the two GNU extensions
/// gcc allows (`void` and a function type, both 1) and every complete array
/// shape -- without these the check could pass by refusing every array.
#[test]
fn diagnostics_sizeof_of_complete_types_is_accepted() {
    compile_expect_ok("sz_int", "int main(void){ return (int)sizeof(int) - 4; }\n");
    compile_expect_ok(
        "sz_ptr",
        "int main(void){ return (int)sizeof(int*) - 8; }\n",
    );
    compile_expect_ok(
        "sz_fixed_arr",
        "int main(void){ return (int)sizeof(int[4]) - 16; }\n",
    );
    compile_expect_ok(
        "sz_vla",
        "int main(void){ int n = 4; return (int)sizeof(int[n]) - 16; }\n",
    );
    compile_expect_ok(
        "sz_vla_2d",
        "int main(void){ int n = 4; return (int)sizeof(int[3][n]) - 48; }\n",
    );
    compile_expect_ok(
        "sz_ptr_to_vla",
        "int main(void){ int n = 4; return (int)sizeof(int(*)[n]) - 8; }\n",
    );
    compile_expect_ok(
        "sz_defined_struct",
        "struct S { int a; };\nint main(void){ return (int)sizeof(struct S) - 4; }\n",
    );
    compile_expect_ok(
        "sz_completed_enum",
        "enum E;\nenum E { A };\nint main(void){ return (int)sizeof(enum E) - 4; }\n",
    );
    // GNU extensions gcc accepts, both giving 1.
    compile_expect_ok(
        "sz_void",
        "int main(void){ return (int)sizeof(void) - 1; }\n",
    );
}

/// `typeof` yields a bare type, so a VLA's extent does not survive it and the
/// result is indistinguishable from an incomplete array. `sizeof(typeof(a))`
/// is legal -- gcc answers with the VLA's size -- so the completeness check
/// above must not fire on it. It answers 0 rather than 16, which is #C89 and
/// unfixed; what this pins is that it is not *rejected*, since `typeof`
/// appears in real system headers.
#[test]
fn diagnostics_sizeof_of_a_typeof_is_not_rejected() {
    // Was `* 0`, written to accommodate the wrong answer #C89 recorded: this
    // gave 0 where gcc gives 16. It is the real size now, so the arithmetic
    // can be the check.
    compile_expect_ok(
        "sz_typeof_vla",
        "int main(void){ int n = 4; int a[n]; return (int)sizeof(typeof(a)) - 16; }\n",
    );
    compile_expect_ok(
        "sz_typeof_fixed",
        "int main(void){ int b[4]; return (int)sizeof(typeof(b)) - 16; }\n",
    );
    compile_expect_ok(
        "sz_typeof_scalar",
        "int main(void){ int x = 0; return (int)sizeof(typeof(x)) - 4; }\n",
    );
}

// ============================================================================
// Array compatibility when one side has no extent (C17 6.7.6.2p6)
// ============================================================================

/// Two array types are compatible if their element types are and *both* have
/// a constant size, in which case the sizes must agree. A side with no extent
/// imposes no size requirement.
///
/// Requiring the extents to be equal made a legal call diagnosed: a parameter
/// `int a[n][m]` decays to `int (*)[m]`, whose pointee has no extent, so
/// passing an ordinary `int m[2][2]` drew "passing argument 3 as 'int[]*'
/// from 'int[2]*' incompatible pointer type" where gcc is silent under -Wall.
#[test]
fn diagnostics_array_of_unspecified_size_is_compatible() {
    compile_expect_ok(
        "vla_param_2d",
        "int f(int n, int m, int a[n][m]) { return a[n-1][m-1]; }\n\
         int main(void){ int m[2][2] = {{1,2},{3,4}}; return f(2,2,m) - 4; }\n",
    );
    compile_expect_ok(
        "vla_param_mixed",
        "int f(int n, int a[3][n]) { return a[0][0]; }\n\
         int main(void){ int m[2][2] = {{1,2},{3,4}}; return f(2,m) - 1; }\n",
    );
    compile_expect_ok(
        "incomplete_array_ptr",
        "void f(int (*p)[]);\nint main(void){ int a[2][3]; f(a); return 0; }\n",
    );
}

/// A genuine element-type mismatch is still diagnosed, and so is a mismatch
/// between two *constant* extents -- without these the rule above could pass
/// by treating every array as compatible with every other.
#[test]
fn diagnostics_incompatible_array_pointers_are_still_diagnosed() {
    compile_expect_warning(
        "arr_elem_mismatch",
        "void f(int (*p)[3]);\nint main(void){ double m[2][3]; f(m); return 0; }\n",
        "incompatible pointer type",
    );
    compile_expect_warning(
        "arr_size_mismatch",
        "void f(int (*p)[3]);\nint main(void){ int m[2][2]; f(m); return 0; }\n",
        "incompatible pointer type",
    );
}

// === #C101 — an address difference within one object is a constant (C17 6.6) ===

/// gcc folds the difference of two addresses into the *same* object, and both
/// `&a[2] - &a[0]` and `(char *)&s.b - (char *)&s.a` are ordinary idioms. c17
/// rejected every one of them, and reported the first at line 0 with no column.
///
/// The accept side matters more than the reject side here: this finding is a
/// *false rejection*, so a fix that merely stopped diagnosing would pass a
/// reject-only test while folding to nonsense. `codegen_static_address_difference`
/// checks the values.
#[test]
fn diagnostics_address_differences_within_one_object_are_accepted() {
    compile_expect_ok("adiff_array", "int a[4];\nlong d = &a[2] - &a[0];\n");
    compile_expect_ok("adiff_decay", "int a[4];\nlong d = (a + 2) - a;\n");
    compile_expect_ok(
        "adiff_member",
        "struct S { int a; int b; };\nstruct S s;\nlong d = (char *)&s.b - (char *)&s.a;\n",
    );
    compile_expect_ok(
        "adiff_cast",
        "int x;\nunsigned long m = (unsigned long)&x - (unsigned long)&x;\n",
    );
    compile_expect_ok(
        "adiff_nested",
        "struct I { int p; int q; };\nstruct O { int head; struct I in; };\nstruct O o;\n\
         long d = (char *)&o.in.q - (char *)&o.head;\n",
    );
}

/// The distance between two *different* objects is not known until they are
/// laid out, so it is not a constant and gcc rejects it. Without these the fix
/// could pass by folding any subtraction it could reach a symbol through --
/// including one that reads two ordinary variables, which is no kind of
/// constant at all.
#[test]
fn diagnostics_address_differences_across_objects_are_rejected() {
    compile_expect_error(
        "adiff_two_arrays",
        "int a[4];\nint b[4];\nlong d = &a[0] - &b[0];\n",
        "constant",
    );
    compile_expect_error(
        "adiff_two_casts",
        "int x;\nint y;\nunsigned long m = (unsigned long)&x - (unsigned long)&y;\n",
        "constant",
    );
    compile_expect_error(
        "adiff_two_objects",
        "struct S { int a; };\nstruct S s;\nstruct S t;\nlong d = (char *)&t.a - (char *)&s.a;\n",
        "constant",
    );
    // Two variables read for their values, which merely happens to be spelled
    // as a subtraction.
    compile_expect_error(
        "adiff_two_vars",
        "int v;\nint w;\nlong d = v - w;\n",
        "constant",
    );
    // Two pointer *objects*, whose values are not known either.
    compile_expect_error(
        "adiff_two_ptrs",
        "int *p;\nint *q;\nlong d = p - q;\n",
        "constant",
    );
}

// === #C105 — bit-field types (C17 6.7.2.1p5) ===

/// An enumeration is a bit-field type gcc accepts and real headers rely on.
#[test]
fn diagnostics_enum_bitfields_are_accepted() {
    compile_expect_ok("bf_enum", "enum E { A, B };\nstruct S { enum E e : 2; };\n");
    compile_expect_ok(
        "bf_enum_unnamed",
        "enum E { A, B };\nstruct S { enum E : 2; };\n",
    );
    compile_expect_ok(
        "bf_enum_mixed",
        "enum E { A, B };\nstruct S { enum E e : 2; unsigned u : 3; int i; };\n",
    );
    compile_expect_ok("bf_bool", "struct S { _Bool b : 1; };\n");
    compile_expect_ok("bf_typedef", "typedef int my;\nstruct S { my m : 3; };\n");
    compile_expect_ok("bf_longlong", "struct S { long long v : 40; };\n");
}

/// A bit-field still may not have a non-integer type -- and the *unnamed* form
/// was validating nothing at all, so `struct { float : 3; }` was accepted where
/// the named spelling was rejected.
#[test]
fn diagnostics_non_integer_bitfields_are_rejected() {
    for (name, src) in [
        ("bf_float_named", "struct S { float f : 3; };\n"),
        ("bf_float_unnamed", "struct S { float : 3; };\n"),
        ("bf_double_unnamed", "struct S { double : 3; };\n"),
        ("bf_ptr", "struct S { int *p : 3; };\n"),
        (
            "bf_struct",
            "struct I { int x; };\nstruct S { struct I i : 3; };\n",
        ),
    ] {
        compile_expect_error(name, src, "bitfield must have integer type");
    }
    // Width constraints still apply to the unnamed form too.
    compile_expect_error(
        "bf_unnamed_wide",
        "struct S { int : 40; };\n",
        "exceeds type size",
    );
}

// === #C106 — jump and label statement constraints (C17 6.8.1, 6.8.4.2, 6.8.6) ===

/// Four constraints that were accepted in silence, each producing a broken
/// program rather than merely a missing message.
///
/// A `goto` to a label that does not exist minted a basic block for it and
/// never terminated it, so control fell out of the function through whatever
/// happened to follow in layout order: the program built, linked, and
/// **segfaulted**. Two labels of one name were merged into a single block, so
/// `L: i++; if (i<2) goto L; L: return i;` **looped forever**. A `break` or
/// `continue` with nothing to jump out of was silently *deleted* -- the control
/// flow the program asked for simply did not happen. And a `switch` on a
/// non-integer compiled and took the wrong branch, comparing the value's bit
/// pattern.
#[test]
fn diagnostics_stray_jumps_and_labels_are_rejected() {
    compile_expect_error(
        "goto_undefined",
        "int f(void){ goto nowhere; return 0; }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "label_duplicate",
        "int f(void){ L: ; L: ; return 0; }\n",
        "duplicate label 'L'",
    );
    compile_expect_error(
        "break_outside",
        "int f(void){ break; return 0; }\n",
        "break statement not within loop or switch",
    );
    compile_expect_error(
        "break_outside_nested_block",
        "int f(void){ { { break; } } return 0; }\n",
        "break statement not within loop or switch",
    );
    compile_expect_error(
        "continue_outside",
        "int f(void){ continue; return 0; }\n",
        "continue statement not within a loop",
    );
    compile_expect_error(
        "case_outside",
        "int f(void){ case 1: return 0; }\n",
        "case label not within a switch statement",
    );
    compile_expect_error(
        "default_outside",
        "int f(void){ default: return 0; }\n",
        "'default' label not within a switch statement",
    );
    compile_expect_error(
        "switch_on_double",
        "int f(double d){ switch(d){ case 1: return 1; } return 0; }\n",
        "switch quantity is not an integer",
    );
    compile_expect_error(
        "switch_on_struct",
        "struct S { int a; };\nint f(struct S s){ switch(s){ case 1: return 1; } return 0; }\n",
        "switch quantity is not an integer",
    );
}

/// The accept side, which is where a check of this shape actually goes wrong.
/// Every row is a construct gcc accepts and an over-eager version of the check
/// would reject: a *forward* `goto` names a label the walk has not reached yet;
/// a label is scoped to its function, so the same name in two functions is not
/// a duplicate; a `switch` supplies a `break` target but not a `continue` one,
/// so a `continue` inside a switch belongs to the loop around it; a statement
/// expression is transparent to `break`; and `switch` accepts every integer
/// type, `enum`, `char`, `_Bool`, a bit-field and `__int128` included.
#[test]
fn diagnostics_legal_jumps_and_labels_are_accepted() {
    for (name, src) in [
        ("goto_forward", "int f(void){ goto L; L: return 0; }\n"),
        (
            "goto_backward",
            "int f(void){ int i=0; L: i++; if(i<2) goto L; return i; }\n",
        ),
        ("goto_out_of_block", "int f(void){ { goto L; } L: return 0; }\n"),
        (
            "label_same_name_two_functions",
            "int f(void){ L: return 0; }\nint g(void){ L: return 1; }\n",
        ),
        ("break_in_while", "int f(void){ while(1){ break; } return 0; }\n"),
        ("break_in_do", "int f(void){ do { break; } while(0); return 0; }\n"),
        (
            "break_in_switch",
            "int f(int x){ switch(x){ case 1: break; } return 0; }\n",
        ),
        (
            "break_nested_loops",
            "int f(void){ while(1){ while(1){ break; } break; } return 0; }\n",
        ),
        (
            "break_in_statement_expression",
            "int f(void){ for(;;){ ({ break; }); } return 0; }\n",
        ),
        (
            "continue_in_for",
            "int f(void){ for(int i=0;i<3;i++){ continue; } return 0; }\n",
        ),
        (
            "continue_through_switch",
            "int f(void){ for(int i=0;i<3;i++){ switch(i){ case 1: continue; } } return 0; }\n",
        ),
        (
            "switch_on_enum",
            "enum E { A, B };\nint f(enum E e){ switch(e){ case A: return 1; } return 0; }\n",
        ),
        ("switch_on_char", "int f(char c){ switch(c){ case 1: return 1; } return 0; }\n"),
        ("switch_on_bool", "int f(_Bool b){ switch(b){ case 1: return 1; } return 0; }\n"),
        (
            "switch_on_bitfield",
            "struct S { int b:5; };\nint f(struct S s){ switch(s.b){ case 1: return 1; } return 0; }\n",
        ),
        (
            "switch_on_int128",
            "int f(__int128 v){ switch(v){ case 1: return 1; } return 0; }\n",
        ),
        (
            "duffs_device",
            "int f(int n, int *p){ switch(n%4){ case 0: do { (*p)++; case 3: (*p)++; \
             case 2: (*p)++; case 1: (*p)++; } while((n-=4)>0); } return 0; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// === #C107 — operand constraints on `*` and on a call (C17 6.5.3.2p2, 6.5.2.2p1) ===

/// Both were accepted in silence and both produced a broken program.
///
/// `int x; *x;` dereferenced the integer's *value* as an address, so the
/// program built and **segfaulted**. `int x; x();` reached the back end and
/// emitted a call to a symbol named `x`, so a front-end error surfaced as an
/// undefined-reference **link** failure naming a variable that plainly exists.
#[test]
fn diagnostics_bad_deref_and_call_operands_are_rejected() {
    for (name, src) in [
        ("deref_int", "int f(void){ int x = 0; return *x; }\n"),
        ("deref_double", "int f(double d){ return (int)*d; }\n"),
        (
            "deref_struct",
            "struct S { int a; };\nint f(struct S s){ return *s; }\n",
        ),
    ] {
        compile_expect_error(name, src, "invalid type argument of unary '*'");
    }
    for (name, src) in [
        ("call_int", "int f(void){ int x = 0; x(); return 0; }\n"),
        ("call_double", "int f(double d){ d(); return 0; }\n"),
        (
            "call_struct",
            "struct S { int a; };\nint f(struct S s){ s(); return 0; }\n",
        ),
    ] {
        compile_expect_error(
            name,
            src,
            "called object is not a function or function pointer",
        );
    }
}

/// The accept side. A *function designator* may be dereferenced -- `(*f)()` and
/// even `(***f)()` are ordinary idioms gcc accepts, `*f` on a function being a
/// no-op -- and a call may go through a pointer, a cast to one, a struct
/// member, or an array element. Rejecting any of these would break far more
/// code than the checks above fix.
#[test]
fn diagnostics_legal_deref_and_call_operands_are_accepted() {
    for (name, src) in [
        ("deref_pointer", "int f(int *p){ return *p; }\n"),
        (
            "deref_array",
            "int f(void){ int a[2] = {1,2}; return *a; }\n",
        ),
        ("deref_ptr_to_ptr", "int f(int **p){ return **p; }\n"),
        ("deref_void_ptr", "int f(void *p){ *p; return 0; }\n"),
        (
            "deref_function_designator",
            "int g(void);\nint f(void){ return (*g)(); }\n",
        ),
        (
            "deref_function_thrice",
            "int g(void);\nint f(void){ return (***g)(); }\n",
        ),
        (
            "call_function",
            "int g(void);\nint f(void){ return g(); }\n",
        ),
        ("call_pointer", "int f(int (*fp)(void)){ return fp(); }\n"),
        (
            "call_deref_pointer",
            "int f(int (*fp)(void)){ return (*fp)(); }\n",
        ),
        (
            "call_deref_thrice",
            "int f(int (*fp)(void)){ return (***fp)(); }\n",
        ),
        (
            "call_cast",
            "int f(void *p){ return ((int (*)(void))p)(); }\n",
        ),
        (
            "call_member",
            "struct S { int (*fp)(void); };\nint f(struct S s){ return s.fp(); }\n",
        ),
        (
            "call_array_element",
            "int f(int (*a[2])(void)){ return a[0](); }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// === #C108 — initializer constraints (C17 6.7.9p11, p13, p14) ===

/// Nothing checked an initializer's type, at either scope.
///
/// The *assignment* form of every case below was already diagnosed by #C47, so
/// the same program was accepted or rejected depending on whether the value
/// arrived through `=` in a declaration or `=` in a statement. And these are
/// not merely undiagnosed: `int x = s;` from a struct and `int b[3] = a;` from
/// an array both compiled and yielded **zero**, while `int *q = d;` reached
/// the same `emit_convert` that #C47 stopped a plain assignment from taking.
///
/// Severities come from `AssignFault::is_error`, so they are gcc's without a
/// table to maintain: an incompatible type is an error and the pointer/integer
/// conversions are warnings.
#[test]
fn diagnostics_bad_initializers_are_rejected() {
    for (name, src, expected) in [
        (
            "init_scalar_from_struct",
            "struct S { int a; };\nvoid f(struct S s){ int x = s; (void)x; }\n",
            "incompatible types when initializing",
        ),
        (
            "init_struct_from_scalar",
            "struct S { int a; };\nvoid f(void){ struct S s = 1; (void)s; }\n",
            "invalid initializer",
        ),
        (
            "init_array_from_array",
            "void f(void){ int a[3]; int b[3] = a; (void)b; }\n",
            "invalid initializer",
        ),
        (
            "init_ptr_from_double",
            "void f(double d){ int *q = d; (void)q; }\n",
            "incompatible types when initializing",
        ),
        // The same four at file scope, which is a separate declarator path.
        (
            "init_fs_scalar_from_struct",
            "struct S { int a; };\nstruct S s;\nint x = s;\n",
            "incompatible types when initializing",
        ),
        (
            "init_fs_struct_from_scalar",
            "struct S { int a; };\nstruct S s = 1;\n",
            "invalid initializer",
        ),
        (
            "init_fs_array_from_array",
            "int a[3];\nint b[3] = a;\n",
            "invalid initializer",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// gcc only warns for the pointer/integer conversions, so c17 must too --
/// erroring here would reject a great deal of code that builds everywhere.
#[test]
fn diagnostics_converting_initializers_only_warn() {
    compile_expect_warning(
        "init_incompatible_pointer",
        "void f(int *p){ char *q = p; (void)q; }\n",
        "incompatible pointer type",
    );
    compile_expect_warning(
        "init_int_from_pointer",
        "void f(int *p){ int b = p; (void)b; }\n",
        "makes integer from pointer",
    );
    compile_expect_warning(
        "init_pointer_from_int",
        "void f(void){ int *p = 7; (void)p; }\n",
        "makes pointer from integer",
    );
}

/// The accept side. An aggregate may be initialized from a compatible
/// aggregate, a character array from a string literal with or without braces,
/// and every ordinary conversion still applies -- so a check of this shape has
/// far more ways to be too strict than too lax.
#[test]
fn diagnostics_ordinary_initializers_are_accepted() {
    for (name, src) in [
        (
            "init_struct_from_struct",
            "struct S { int a; };\nvoid f(struct S b){ struct S s = b; (void)s; }\n",
        ),
        (
            "init_union_from_union",
            "union U { int a; };\nvoid f(union U b){ union U u = b; (void)u; }\n",
        ),
        (
            "init_char_array",
            "void f(void){ char s[6] = \"hello\"; (void)s; }\n",
        ),
        (
            "init_wide_array",
            "void f(void){ __WCHAR_TYPE__ s[3] = L\"ab\"; (void)s; }\n",
        ),
        (
            "init_braced_string",
            "void f(void){ char s[6] = {\"hello\"}; (void)s; }\n",
        ),
        ("init_scalar", "void f(void){ int x = 3; (void)x; }\n"),
        ("init_widening", "void f(void){ double d = 3; (void)d; }\n"),
        (
            "init_null_pointer",
            "void f(void){ int *p = 0; (void)p; }\n",
        ),
        (
            "init_from_void_ptr",
            "void f(void *v){ int *p = v; (void)p; }\n",
        ),
        (
            "init_to_void_ptr",
            "void f(int *q){ void *p = q; (void)p; }\n",
        ),
        (
            "init_bool_from_ptr",
            "void f(int *p){ _Bool b = p; (void)b; }\n",
        ),
        (
            "init_struct_brace",
            "struct S { int a; };\nvoid f(void){ struct S s = {1}; (void)s; }\n",
        ),
        (
            "init_array_brace",
            "void f(void){ int b[3] = {1,2,3}; (void)b; }\n",
        ),
        (
            "init_scalar_brace",
            "void f(void){ int x = {3}; (void)x; }\n",
        ),
        (
            "init_array_of_struct",
            "struct S { int a; };\nvoid f(void){ struct S v[2] = {{1},{2}}; (void)v; }\n",
        ),
        (
            "init_designated",
            "void f(void){ int a[5] = {[3]=9}; (void)a; }\n",
        ),
        (
            "init_string_pointer",
            "void f(void){ const char *s = \"hi\"; (void)s; }\n",
        ),
        (
            "init_function_pointer",
            "int g(void);\nvoid f(void){ int (*fp)(void) = g; (void)fp; }\n",
        ),
        (
            "init_array_decay",
            "void f(void){ int a[3]; int *p = a; (void)p; }\n",
        ),
        (
            "init_compound_literal",
            "void f(void){ int *p = (int[]){1,2}; (void)p; }\n",
        ),
        (
            "init_enum",
            "enum E { A, B };\nvoid f(void){ enum E e = A; (void)e; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// === #C109 — declaration constraints (C17 6.7p3, 6.7.1p2, 6.7.9p5, 6.9.1p5) ===

/// Four more constraints accepted in silence, each losing information rather
/// than merely a message.
///
/// A repeated enumerator kept the first one's value. A repeated parameter left
/// the second with no symbol at all, so every use of the name reached the
/// first. `static static int x;` and `static extern int x;` were both taken,
/// the modifier bits simply being OR-ed together. And `extern int e = 1;` at
/// *block* scope declared a name with linkage and then defined it locally.
#[test]
fn diagnostics_repeated_declaration_parts_are_rejected() {
    compile_expect_error(
        "decl_dup_enumerator",
        "enum A { X };\nenum B { X };\n",
        "redeclaration of enumerator 'X'",
    );
    compile_expect_error(
        "decl_enumerator_over_variable",
        "int Y;\nenum A { Y };\n",
        "redeclared as a different kind of symbol",
    );
    compile_expect_error(
        "decl_variable_over_enumerator",
        "enum A { Z };\nint Z;\n",
        "redeclared as a different kind of symbol",
    );
    compile_expect_error(
        "decl_dup_parameter",
        "void f(int a, int a){ (void)a; }\n",
        "redefinition of parameter 'a'",
    );
    compile_expect_error(
        "decl_dup_parameter_third",
        "void f(int a, int b, int a){ (void)a; (void)b; }\n",
        "redefinition of parameter 'a'",
    );
    compile_expect_error(
        "decl_static_static",
        "static static int x;\n",
        "duplicate 'static'",
    );
    compile_expect_error(
        "decl_extern_extern",
        "extern extern int x;\n",
        "duplicate 'extern'",
    );
    compile_expect_error(
        "decl_static_extern",
        "static extern int x;\n",
        "multiple storage classes",
    );
    compile_expect_error(
        "decl_typedef_static",
        "typedef static int T;\n",
        "multiple storage classes",
    );
    compile_expect_error(
        "decl_extern_init_block",
        "void f(void){ extern int e = 1; (void)e; }\n",
        "'extern' variable has an initializer",
    );
}

/// At *file* scope the same `extern` spelling is a definition with external
/// linkage, so gcc warns rather than rejecting -- the scope is the whole
/// distinction.
#[test]
fn diagnostics_file_scope_extern_initializer_only_warns() {
    compile_expect_warning(
        "decl_extern_init_file",
        "extern int e = 1;\n",
        "'extern' variable has an initializer",
    );
}

/// The accept side. `_Thread_local` may accompany `static` or `extern` in
/// either order, so counting it as a storage class would reject ordinary
/// thread-local declarations; a qualifier may repeat where a storage class may
/// not; an enumerator may be shadowed in an inner scope; and an unnamed
/// parameter is not a duplicate of another unnamed one.
#[test]
fn diagnostics_ordinary_declarations_are_accepted() {
    for (name, src) in [
        ("decl_thread_local_static", "_Thread_local static int x;\n"),
        ("decl_static_thread_local", "static _Thread_local int x;\n"),
        ("decl_extern_thread_local", "extern _Thread_local int x;\n"),
        ("decl_const_const", "const const int x = 1;\n"),
        ("decl_static_const", "static const int x = 1;\n"),
        (
            "decl_static_inline",
            "static inline int f(void){ return 1; }\n",
        ),
        ("decl_unnamed_params", "void f(int, int);\n"),
        (
            "decl_named_prototype_and_definition",
            "void f(int a, int b);\nvoid f(int a, int b){ (void)a; (void)b; }\n",
        ),
        (
            "decl_register_param",
            "void f(register int a){ (void)a; }\n",
        ),
        (
            "decl_enumerator_shadowed",
            "enum A { X };\nvoid f(void){ enum B { X }; (void)X; }\n",
        ),
        (
            "decl_distinct_enumerators",
            "enum A { P, Q };\nenum B { R, S };\n",
        ),
        ("decl_enumerator_used", "enum A { V };\nint q = V;\n"),
        (
            "decl_extern_then_local_decl",
            "extern int e;\nvoid f(void){ extern int e; (void)e; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// === #C111 — incomplete types and flexible array members (6.7p7, 6.7.2.1p18) ===

/// An object needs a size, and a flexible array member has a place.
///
/// Neither was checked: `struct U; struct U u;` compiled and `sizeof` it
/// answered 0, and nothing in the tree recognised a flexible array member at
/// all, so `struct S { int a[]; int b; }` sized the array zero and carried on.
#[test]
fn diagnostics_incomplete_objects_and_misplaced_flexible_arrays_are_rejected() {
    for (name, src) in [
        (
            "inc_block_object",
            "struct U;\nvoid f(void){ struct U u; (void)&u; }\n",
        ),
        ("inc_file_object", "struct U;\nstruct U u;\n"),
        ("inc_file_union", "union V;\nunion V v;\n"),
    ] {
        compile_expect_error(name, src, "storage size of an object");
    }
    for (name, src) in [
        // An array's element type must be complete where the array is
        // declared: the stride is what forms the type, so this holds even
        // when the tag is completed later and even for `extern`.
        (
            "inc_array_completed_later",
            "struct U;\nstruct U a[2];\nstruct U { int a; };\n",
        ),
        (
            "inc_array_2d",
            "struct U;\nstruct U a[2][3];\nstruct U { int a; };\n",
        ),
        ("inc_array_extern", "struct U;\nextern struct U a[2];\n"),
    ] {
        compile_expect_error(name, src, "incomplete element type");
    }
    compile_expect_error(
        "fam_not_last",
        "struct S { int a[]; int b; };\n",
        "flexible array member not at end of struct",
    );
    compile_expect_error(
        "fam_then_two",
        "struct S { int n; int a[]; int b; };\n",
        "flexible array member not at end of struct",
    );
    compile_expect_error(
        "fam_sole_member",
        "struct S { int a[]; };\n",
        "flexible array member in a struct with no named members",
    );
    compile_expect_error(
        "fam_in_union",
        "union U { int a[]; int b; };\n",
        "flexible array member in union",
    );
}

/// The accept side, and it is the whole difficulty.
///
/// 6.9.2p3 lets a file-scope *tentative* definition be completed later in the
/// translation unit, so the check cannot run where the declaration appears --
/// forward-declare-then-complete is everywhere in CPython and glibc. An
/// `extern` declaration and a pointer define nothing and need no size. And a
/// mid-struct `char d[0]` is a GNU zero-length array, not a flexible array
/// member: conflating the two would reject far more than the check catches.
#[test]
fn diagnostics_complete_objects_and_valid_flexible_arrays_are_accepted() {
    for (name, src) in [
        (
            "inc_tentative_completed",
            "struct U;\nstruct U u;\nstruct U { int a; };\n",
        ),
        (
            "inc_tentative_completed_much_later",
            "struct U;\nstruct U u;\nvoid f(void);\nstruct U { int a; };\nvoid f(void){}\n",
        ),
        (
            "inc_static_tentative_completed",
            "struct U;\nstatic struct U u;\nstruct U { int a; };\n",
        ),
        ("inc_extern_only", "struct U;\nextern struct U u;\n"),
        (
            "inc_extern_block",
            "struct U;\nvoid f(void){ extern struct U u; (void)&u; }\n",
        ),
        ("inc_pointer_only", "struct U;\nstruct U *p;\n"),
        (
            "inc_pointer_param",
            "struct U;\nvoid f(struct U *p){ (void)p; }\n",
        ),
        ("inc_typedef_only", "struct U;\ntypedef struct U T;\n"),
        ("inc_function_returning", "struct U;\nstruct U f(void);\n"),
        (
            "inc_array_of_complete",
            "struct U { int a; };\nstruct U a[2];\n",
        ),
        ("inc_array_of_pointers", "struct U;\nstruct U *a[2];\n"),
        // Flexible array members, valid.
        ("fam_valid", "struct S { int n; int a[]; };\n"),
        (
            "fam_valid_two_before",
            "struct S { int n; char c; int a[]; };\n",
        ),
        (
            "fam_after_bitfield",
            "struct S { unsigned f:3; int a[]; };\n",
        ),
        (
            "fam_typedef",
            "typedef struct { int n; char s[]; } T;\nT *p;\n",
        ),
        (
            "fam_nested_last",
            "struct I { int n; int a[]; };\nstruct O { int x; struct I i; };\n",
        ),
        (
            "fam_array_of_structs",
            "struct I { int n; int a[]; };\nstruct I arr[2];\n",
        ),
        (
            "fam_sizeof",
            "struct S { int n; int a[]; };\nunsigned long x = sizeof(struct S);\n",
        ),
        // GNU zero-length arrays, which are not flexible array members.
        ("zla_mid_struct", "struct S { int n; char d[0]; int t; };\n"),
        ("zla_last", "struct S { int n; char d[0]; };\n"),
        ("zla_sole", "struct S { char d[0]; };\n"),
        ("array_sized_last", "struct S { int n; int a[4]; };\n"),
        ("ptr_to_unsized_array", "struct S { int (*p)[]; int b; };\n"),
    ] {
        compile_expect_ok(name, src);
    }
}

// === Review follow-ups to the 2026-08-18 series ===

/// 6.7.9p14 gives a *character* array the narrow string literal, and p15 gives
/// a wide one an array whose element type is *compatible* with the literal's.
///
/// The first version of #C108's check accepted any string literal for any
/// array, so `int a[] = "hi";` compiled. The distinction p15 draws is finer
/// than "is it a character type?": `int a[] = L"ab";` is legal where `wchar_t`
/// is `int`, while `unsigned a[] = L"ab";` is not -- and all three of `char`,
/// `signed char` and `unsigned char` take the narrow literal, so comparing the
/// element types for strict compatibility would reject two of them.
#[test]
fn diagnostics_string_literal_must_match_the_array_element_type() {
    for (name, src) in [
        ("str_into_int_array", "int a[] = \"hi\";\n"),
        ("str_into_short_array", "short a[] = \"hi\";\n"),
        ("str_into_double_array", "double a[] = \"hi\";\n"),
        (
            "str_into_struct_array",
            "struct S { int a; };\nstruct S s[] = \"hi\";\n",
        ),
        (
            "str_into_local_int_array",
            "void f(void){ int a[] = \"hi\"; (void)a; }\n",
        ),
        // A wide literal needs its own element type, not merely a wide one.
        ("wide_into_char_array", "char a[] = L\"ab\";\n"),
        // wchar_t is `int` on x86-64 and Darwin and `unsigned int` on aarch64
        // Linux, so the mismatch is the integer type of the other signedness.
        (
            "wide_into_other_signedness_array",
            "#if __WCHAR_MIN__ == 0\nint a[] = L\"ab\";\n#else\nunsigned a[] = L\"ab\";\n#endif\n",
        ),
        ("u16_into_char_array", "char a[] = u\"ab\";\n"),
        ("u16_into_short_array", "short a[] = u\"ab\";\n"),
        ("u32_into_int_array", "int a[] = U\"ab\";\n"),
        // `u8"..."` has type char[], so it is narrow.
        ("u8_into_int_array", "int a[] = u8\"ab\";\n"),
    ] {
        compile_expect_error(name, src, "invalid initializer");
    }
}

/// The accept side, which is what rules out the obvious over-strict fix: every
/// character type takes the narrow literal, a qualifier changes nothing, and
/// each wide literal has exactly one element type that suits it.
#[test]
fn diagnostics_string_literals_matching_their_array_are_accepted() {
    for (name, src) in [
        ("str_char", "char a[] = \"hi\";\n"),
        ("str_signed_char", "signed char a[] = \"hi\";\n"),
        ("str_unsigned_char", "unsigned char a[] = \"hi\";\n"),
        ("str_const_char", "const char a[] = \"hi\";\n"),
        ("str_sized", "char a[5] = \"hi\";\n"),
        ("str_braced", "char a[] = {\"hi\"};\n"),
        ("str_u8", "char a[] = u8\"ab\";\n"),
        ("str_local", "void f(void){ char a[] = \"hi\"; (void)a; }\n"),
        ("wide_into_wchar", "__WCHAR_TYPE__ a[] = L\"ab\";\n"),
        (
            "wide_into_const_wchar",
            "const __WCHAR_TYPE__ a[] = L\"ab\";\n",
        ),
        ("u16_into_ushort", "unsigned short a[] = u\"ab\";\n"),
        ("u32_into_uint", "unsigned int a[] = U\"ab\";\n"),
        ("array_from_braces", "int a[] = {1,2,3};\n"),
        ("pointer_from_string", "const char *p = \"hi\";\n"),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A bit-field wider than 64 bits is carried, provided it gets a whole
/// 16-byte storage unit.
///
/// It used to be refused outright at any width above 64: the value mask was a
/// `u64` and `bitfield_storage_type` had no arm for a sixteen-byte unit, so
/// `unsigned __int128 a:100` read back a wrong value in a release build and
/// **panicked the compiler** in a debug one. Both halves exist now, and the
/// carrier's *kind* is `Int128`, which is what routes it to a 16-byte stack
/// slot rather than a GP register the backend cannot address as a pair.
///
/// What remains refused is the packed case, and only that. Packing gives a
/// field an access span of just the bytes its own bits touch, which sends it
/// to the byte-wise path -- and that assembles into a 64-bit carrier, so it
/// cannot hold the value. gcc packs these; c17 says so instead of guessing.
#[test]
fn diagnostics_wide_bitfield_without_a_carrier_is_rejected() {
    for (name, src) in [
        (
            "bf_packed_attr",
            "struct __attribute__((packed)) S { char c; __int128 a:100; };\n",
        ),
        (
            "bf_packed_pragma",
            "#pragma pack(1)\nstruct S { char c; __int128 a:100; };\n",
        ),
    ] {
        compile_expect_error(name, src, "needs an unpacked 16-byte storage unit");
    }

    // Wider than the declared type is a different fault, and keeps its own
    // message -- 6.7.2.1p4 is a constraint, not a c17 limitation.
    compile_expect_error(
        "bf_over_type",
        "struct S { __int128 a:129; };\n",
        "exceeds type size",
    );
}

/// The accept side: every width up to the type's own now compiles.
#[test]
fn diagnostics_bitfields_within_the_carrier_are_accepted() {
    for (name, src) in [
        // The widths this used to refuse.
        ("bf_i128_65", "struct S { unsigned __int128 a:65; };\n"),
        ("bf_i128_100", "struct S { unsigned __int128 a:100; };\n"),
        ("bf_i128_128", "struct S { unsigned __int128 a:128; };\n"),
        ("bf_i128_signed", "struct S { __int128 a:96; };\n"),
        ("bf_i128_unnamed", "struct S { unsigned __int128 : 96; };\n"),
        // ...and the ones that always worked, which must not regress.
        ("bf_i128_64", "struct S { unsigned __int128 a:64; };\n"),
        ("bf_i128_32", "struct S { unsigned __int128 a:32; };\n"),
        ("bf_i128_1", "struct S { unsigned __int128 a:1; };\n"),
        ("bf_ull_64", "struct S { unsigned long long a:64; };\n"),
        ("bf_int_32", "struct S { int a:32; };\n"),
        // A packed field at or below 64 bits still takes the byte-wise path
        // and is fine there, so the new refusal must not catch it.
        (
            "bf_packed_narrow",
            "struct __attribute__((packed)) S { char c; __int128 a:64; };\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A jump into a variably modified scope is reported *at the jump*, where gcc
/// points, and still names the declaration that could not be entered.
///
/// It used to report at the declaration, for want of anything better: the jump
/// and label statements carried no position until #C106 gave them one, and the
/// doc comment justifying the choice outlived the reason. The line and column
/// are asserted here because the position *is* the fix.
#[test]
fn diagnostics_variably_modified_jumps_point_at_the_jump() {
    compile_expect_error(
        "vm_goto_position",
        "int main(void){\n  int n = 4;\n  goto L;\n  {\n    int a[n];\n    L: return a[0];\n  }\n}\n",
        ":3:3: error: jump into the scope of 'a'",
    );
    compile_expect_error(
        "vm_switch_position",
        "int main(void){\n  int n=4, k=1;\n  switch (k) {\n    int a[n];\n    case 1:\n      return a[0];\n  }\n  return 0;\n}\n",
        ":5:10: error: switch jump into the scope of 'a'",
    );
    compile_expect_error(
        "undefined_label_position",
        "int f(void){\n  int x = 1;\n  goto nowhere;\n  return x;\n}\n",
        ":3:3: error: label 'nowhere' used but not defined",
    );
}

// === #C112 — `sizeof` of an incomplete array expression (C17 6.5.3.4p1) ===

/// #C90 closed the type-name form and left this one: `extern int a[]; sizeof a`
/// compiled and answered **0**.
///
/// Neither the type nor the completeness helpers can settle it, because
/// `int[]`, `int[n]` and `int[m]` all intern to one `TypeId` -- the extent
/// lives on the declarator. `Symbol::array_is_variably_modified` records
/// whether one was given, so a VLA's `sizeof` keeps working while an
/// incomplete array's is refused.
#[test]
fn diagnostics_sizeof_of_an_incomplete_array_expression_is_rejected() {
    for (name, src) in [
        (
            "szx_extern_file",
            "extern int a[];\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_extern_parens",
            "extern int a[];\nunsigned long f(void){ return sizeof(a); }\n",
        ),
        (
            "szx_extern_block",
            "unsigned long f(void){ extern int a[]; return sizeof a; }\n",
        ),
        (
            "szx_tentative",
            "int a[];\nunsigned long f(void){ return sizeof a; }\n",
        ),
    ] {
        compile_expect_error(name, src, "incomplete type");
    }
}

/// The accept side. A VLA is measured at run time, a GNU zero-length array has
/// an extent that happens to be zero, and a later declaration completes an
/// earlier `extern int a[];` (6.2.7p4) -- all of which an over-eager check
/// would refuse.
#[test]
fn diagnostics_sizeof_of_complete_array_expressions_is_accepted() {
    for (name, src) in [
        (
            "szx_vla",
            "unsigned long f(int n){ int a[n]; return sizeof a; }\n",
        ),
        (
            "szx_vla_2d",
            "unsigned long f(int n){ int a[n][3]; return sizeof a; }\n",
        ),
        (
            "szx_zero_length",
            "int a[0];\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_sized",
            "int a[4];\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_inferred",
            "int a[] = {1,2,3};\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_string",
            "char a[] = \"hi\";\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_completed_later",
            "extern int a[];\nint a[4];\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_completed_by_init",
            "extern int a[];\nint a[] = {1,2,3};\nunsigned long f(void){ return sizeof a; }\n",
        ),
        (
            "szx_local_fixed",
            "unsigned long f(void){ int a[4]; return sizeof a; }\n",
        ),
        (
            "szx_param_decayed",
            "unsigned long f(int a[]){ return sizeof a; }\n",
        ),
        (
            "szx_2d_file",
            "int a[2][3];\nunsigned long f(void){ return sizeof a; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// === C11 6.5.2.3p5 — naming a member of an atomic structure or union ===

/// Reading or writing one member of an `_Atomic` aggregate touches part of an
/// object whose atomicity covers all of it, so the lock the type promises is
/// not taken. C11 makes it undefined behaviour rather than a constraint
/// violation, so gcc warns and so does c17 now -- it used to say nothing at
/// all, which meant the one operation `_Atomic` exists to prevent was the one
/// it did not mention.
#[test]
fn diagnostics_member_of_an_atomic_aggregate_warns() {
    for (name, src, expected) in [
        (
            "atomic_member_read",
            "struct S { int a; };\n_Atomic struct S s;\nint f(void){ return s.a; }\n",
            "accessing a member 'a' of an atomic structure",
        ),
        (
            "atomic_member_write",
            "struct S { int a; };\n_Atomic struct S s;\nvoid f(void){ s.a = 1; }\n",
            "accessing a member 'a' of an atomic structure",
        ),
        (
            "atomic_member_arrow",
            "struct S { int a; };\nvoid f(_Atomic struct S *p){ (void)p->a; }\n",
            "accessing a member 'a' of an atomic structure",
        ),
        (
            "atomic_member_address",
            "struct S { int a; };\n_Atomic struct S s;\nint *f(void){ return &s.a; }\n",
            "accessing a member 'a' of an atomic structure",
        ),
        (
            "atomic_union_member",
            "union U { int a; };\n_Atomic union U u;\nint f(void){ return u.a; }\n",
            "accessing a member 'a' of an atomic union",
        ),
        (
            // gcc names the outer member, not the inner one.
            "atomic_nested_member",
            "struct I { int q; };\nstruct S { struct I i; };\n_Atomic struct S s;\n\
             int f(void){ return s.i.q; }\n",
            "accessing a member 'i' of an atomic structure",
        ),
        (
            "atomic_through_typedef",
            "struct S { int a; };\ntypedef _Atomic struct S AS;\nAS s;\nint f(void){ return s.a; }\n",
            "accessing a member 'a' of an atomic structure",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

/// It is the *object's* atomicity that matters, not the member's:
/// `struct { _Atomic int a; } s; s.a` is an ordinary access to an atomic
/// member and must stay silent, as must every access to a non-atomic
/// aggregate.
#[test]
fn diagnostics_ordinary_member_access_stays_silent() {
    for (name, src) in [
        (
            "plain_struct_member",
            "struct S { int a; };\nstruct S s;\nint f(void){ return s.a; }\n",
        ),
        (
            "atomic_scalar_member",
            "struct S { _Atomic int a; };\nstruct S s;\nint f(void){ return s.a; }\n",
        ),
        (
            "atomic_scalar_member_arrow",
            "struct S { _Atomic int a; };\nvoid f(struct S *p){ (void)p->a; }\n",
        ),
        (
            "plain_arrow",
            "struct S { int a; };\nvoid f(struct S *p){ (void)p->a; }\n",
        ),
        (
            "atomic_scalar_object",
            "_Atomic int x;\nint f(void){ return x; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// === #C102 — reading an object in a static initializer (C17 6.7.9p4) ===

/// One missing mechanism behind three symptoms, and the middle one is why the
/// first attempt at this was reverted.
///
/// `int v; int w = v;` was accepted and silently yielded **zero**. `const int
/// c = 5; int w = c;` was accepted and right, but only because nothing folded
/// it. And `int w = c + 1;` one step along was **rejected**, valid code that
/// gcc compiles. Fixing any one of them alone leaves the others wrong.
///
/// The folding is scoped to the initializer by a `ConstScope` parameter rather
/// than a flag: C makes a `const` object no kind of constant expression, so it
/// must not reach an array size or a `case` label.
#[test]
fn diagnostics_non_constant_static_initializers_are_rejected() {
    for (name, src) in [
        ("si_read_object", "int v = 5;\nint w = v;\n"),
        ("si_read_in_arithmetic", "int v = 5;\nint w = v + 1;\n"),
        ("si_const_no_initializer", "const int c;\nint w = c;\n"),
        ("si_extern_const", "extern const int c;\nint w = c;\n"),
        ("si_function_call", "int f(void);\nint w = f();\n"),
        (
            "si_static_local",
            "int v;\nvoid f(void){ static int w = v; (void)w; }\n",
        ),
        ("si_float_object", "double v;\ndouble w = v * 2;\n"),
    ] {
        compile_expect_error(name, src, "constant expression");
    }
}

/// A `const` object with a visible initializer folds, in arbitrary arithmetic
/// and at every arithmetic type -- which is what gcc does, silently and even
/// under `-pedantic`.
#[test]
fn diagnostics_const_objects_fold_in_static_initializers() {
    for (name, src) in [
        ("si_const_int", "const int c = 5;\nint w = c;\n"),
        (
            "si_const_arithmetic",
            "const int c = 5;\nint w = c * 2 + 1;\n",
        ),
        ("si_const_negate", "const int c = 5;\nint w = -c;\n"),
        (
            "si_const_conditional",
            "const int c = 5;\nint w = c ? 1 : 2;\n",
        ),
        ("si_const_shift", "const long c = 5;\nlong w = c << 2;\n"),
        (
            "si_const_double",
            "const double d = 2.5;\ndouble x = d * 2;\n",
        ),
        (
            "si_const_float",
            "const float f = 1.5f;\nfloat y = f + 1.0f;\n",
        ),
        ("si_static_const", "static const int c = 7;\nint w = c;\n"),
        (
            "si_const_in_address",
            "const int c = 5;\nint a[10];\nint *p = &a[c - 3];\n",
        ),
        ("si_enum_constant", "enum { N = 7 };\nint w = N;\n"),
        (
            "si_const_array_element",
            "const int a[2] = {1,2};\nint w = a[0];\n",
        ),
        (
            "si_const_struct_member",
            "struct S { int a; };\nconst struct S s = {5};\nint w = s.a;\n",
        ),
        (
            "si_block_scope_auto",
            "int v;\nvoid f(void){ int w = v; (void)w; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// The boundary. C makes a `const` object no kind of constant expression, so
/// the folding must reach the initializer and nowhere else -- `int a[c];` is a
/// VLA and `case c:` an error, in gcc as here. Without these the fix would
/// silently turn five other contexts lax.
#[test]
fn diagnostics_const_objects_do_not_fold_outside_initializers() {
    compile_expect_error(
        "si_boundary_array_size",
        "const int c = 5;\nint a[c];\n",
        "file scope",
    );
    compile_expect_error(
        "si_boundary_case_label",
        "const int c = 5;\nvoid f(int x){ switch(x){ case c: break; } }\n",
        "constant",
    );
    compile_expect_error(
        "si_boundary_static_assert",
        "const int c = 5;\n_Static_assert(c == 5, \"x\");\n",
        "constant",
    );
    compile_expect_error(
        "si_boundary_enumerator",
        "const int c = 5;\nenum E { X = c };\n",
        "constant",
    );
    compile_expect_error(
        "si_boundary_bitfield_width",
        "const int c = 5;\nstruct S { int b : c; };\n",
        "constant",
    );
}

// === #C118 — an out-of-range enumerator must not panic the compiler ===

/// An enumeration with no possible underlying type is diagnosed, not a panic.
///
/// `enum_underlying_type` reached an `unreachable!` whose premise -- that a
/// non-negative maximum always fits `u64` -- nothing enforced, so the compiler
/// exited 101. gcc accepts these by giving the enumeration a `__int128`
/// underlying type, which c17 does not offer; refusing them is a deliberate
/// divergence, and the point of the test is that it is a *diagnostic*.
#[test]
fn diagnostics_unrepresentable_enumeration_does_not_panic() {
    for (name, src) in [
        (
            "wide_positive_and_negative",
            "enum E { A = (__int128)1 << 100, B = -1 };\nint main(void){ return 0; }\n",
        ),
        (
            "wide_positive_alone",
            "enum E { A = (__int128)1 << 100 };\nint main(void){ return 0; }\n",
        ),
    ] {
        // The message is the discriminator: a panic also exits non-zero, so
        // asserting only on failure would have passed against the crash.
        compile_expect_error(name, src, "no integer type can represent all values");
    }
}

/// A folded shift agrees with the same shift computed at run time.
///
/// The count is masked to the operand width, as the hardware does. 6.5.7p3
/// makes a count outside `[0, width)` undefined and gcc has no single answer
/// for it either -- it folds `1 << 64` to 0 but leaves `-1 >> 64` at -1 -- so
/// what this pins is c17's *self*-consistency: an enumerator, an array bound
/// and a run-time expression must not disagree. Recorded at #C125, which is
/// the warning gcc has and c17 does not.
#[test]
fn diagnostics_folded_shift_agrees_with_the_runtime_one() {
    assert_eq!(
        compile_and_run(
            "folded_shift_matches_runtime",
            "enum E { A = 1 << 64, B = 1 >> 64, C = (char)1 << 20, D = 1LL << 40 };\n\
             int main(void) {\n\
             volatile int one = 1, sixty_four = 64;\n\
             if (A != (one << sixty_four)) return 1;\n\
             if (B != (one >> sixty_four)) return 2;\n\
             if (C != 1048576) return 3;\n\
             if (D != 1099511627776LL) return 4;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// Enumerators that do fit are unaffected, at every width the choice of
/// underlying type turns on.
#[test]
fn diagnostics_representable_enumerators_are_accepted() {
    for (name, src) in [
        (
            "enum_int_max",
            "enum E { A = 2147483647 };\nint main(void){ return 0; }\n",
        ),
        (
            "enum_shift_31",
            "enum E { A = 1 << 31 };\nint main(void){ return 0; }\n",
        ),
        (
            "enum_uint_max",
            "enum E { A = 0xFFFFFFFFU };\nint main(void){ return 0; }\n",
        ),
        (
            "enum_ulong_max",
            "enum E { A = 0xFFFFFFFFFFFFFFFFULL };\nint main(void){ return 0; }\n",
        ),
        (
            "enum_negative",
            "enum E { A = -2147483648 };\nint main(void){ return 0; }\n",
        ),
        (
            "enum_plain",
            "enum E { A, B, C };\nint main(void){ return 0; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// An object too large to describe is diagnosed, not capped.
///
/// The bound is what C makes it, and gcc's: an object is addressed by pointer
/// arithmetic, and 6.5.6p9 makes the difference of two pointers into one
/// object a `ptrdiff_t`, so an object whose size does not fit a signed 64-bit
/// value cannot be indexed from end to end. `PTRDIFF_MAX` itself is allowed
/// and one byte more is not. It used to be 512 MB, an accident of `size_bits`
/// answering in a `u32`, and an extent past it saturated in silence:
/// `char big[5000000000];` compiled and reported `sizeof` 536870911. Then it
/// was `u64::MAX / 8`, because struct layout accumulated its bits in a
/// `usize`, which refused gcc.c-torture `991014-1`.
#[test]
fn diagnostics_object_larger_than_the_compiler_can_describe() {
    for (name, src) in [
        (
            "array_past_ptrdiff",
            "char big[9300000000000000000UL];\nint main(void){ return 0; }\n",
        ),
        (
            "array_of_int_past_ptrdiff",
            "int big[4000000000000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_two_dimensions",
            "char big[4000000000L][4000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_block_scope",
            "int main(void){ static char big[9300000000000000000UL]; return big[0]; }\n",
        ),
        (
            "array_typedef",
            "typedef char T[9300000000000000000UL];\nint main(void){ return 0; }\n",
        ),
        (
            "array_one_past_ptrdiff_max",
            "typedef char T[9223372036854775808UL];\nint main(void){ return 0; }\n",
        ),
    ] {
        compile_expect_error(
            name,
            src,
            "size of array is too large: it exceeds the maximum object size of \
             9223372036854775807 bytes",
        );
    }

    // A member list can reach the bound even when no single member does, and
    // a sum past `u64::MAX` bits is measured, not wrapped.
    for (name, src, needle) in [
        (
            "struct_sum_of_members",
            "struct S { char a[5000000000000000000L]; char b[5000000000000000000L]; } s;\n\
             int main(void){ return 0; }\n",
            "type 'struct S' is too large",
        ),
        (
            "struct_trailing_member",
            "struct S { char a[9223372036854775807L]; int b; };\n\
             int main(void){ return 0; }\n",
            "type 'struct S' is too large",
        ),
        (
            "union_rounded_past",
            "union U { char a[9223372036854775807L]; int b; };\n\
             int main(void){ return 0; }\n",
            "type 'union U' is too large",
        ),
    ] {
        compile_expect_error(name, src, needle);
    }
}

/// The largest object that *is* describable keeps working, and `sizeof` agrees
/// with gcc for it — the bound has to be a diagnostic at the edge, not a cap
/// that moved.
#[test]
fn diagnostics_largest_describable_object_is_accepted() {
    for (name, src) in [
        // Past the old 512 MB cap, and well within what C allows.
        (
            "array_past_the_old_cap",
            "char big[2000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_of_int_past_the_old_cap",
            "int big[2000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_two_dimensions",
            "char big[16385][32768];\nint main(void){ return 0; }\n",
        ),
        (
            "struct_sum_past_the_old_cap",
            "struct S { char a[400000000]; char b[400000000]; } s;\nint main(void){ return 0; }\n",
        ),
        (
            "array_near_ptrdiff_max",
            "typedef char T[2000000000000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_at_ptrdiff_max",
            "typedef char T[9223372036854775807L];\nint main(void){ return 0; }\n",
        ),
        (
            "struct_past_u64_bits",
            "struct S { short buf[(1L << 62) - 256]; int a, b, c, d; };\n\
             int main(void){ return sizeof(struct S) != 9223372036854775312UL; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }

    // And the sizes are the ones gcc reports, so that moving the bound cannot
    // quietly move an answer with it.
    //
    // Everything past the old cap is asked of a *type*, not of an object.
    // `sizeof` needs no storage, and defining the objects instead made the
    // program ask its loader for gigabytes of zero-fill: a `char b[2000000000]`
    // here is `.zerofill` of 2 GB in the Mach-O, and macOS refuses to map it
    // ("dyld cache not loaded: syscall to map cache into shared region
    // failed") where Linux's overcommit had hidden the cost. The one object
    // that is defined is the size the old bound allowed, which is what pins
    // that the bound moved without the answers moving.
    assert_eq!(
        compile_and_run(
            "object_sizes_are_exact",
            "char a[536870911];\n\
             struct S { char x[100000000]; char y[100000000]; } s;\n\
             typedef char PastOldCap[2000000000L];\n\
             typedef char Huge[2000000000000000000L];\n\
             typedef struct { char x[4000000000L]; char y[4000000000L]; } BigSum;\n\
             int main(void) {\n\
             if (sizeof a != 536870911UL) return 1;\n\
             if (sizeof s != 200000000UL) return 2;\n\
             if (sizeof (PastOldCap) != 2000000000UL) return 3;\n\
             if (sizeof (Huge) != 2000000000000000000UL) return 4;\n\
             if (sizeof (BigSum) != 8000000000UL) return 5;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// A floating literal is not an integer constant expression.
///
/// 6.6p6 admits one only as the immediate operand of a cast. The parser's
/// folder had a `FloatLit` arm that truncated it instead, so four constraint
/// violations compiled: an array bound, an enumerator, a bit-field width and
/// a `_Static_assert`. At block scope the array case was worse than accepted
/// -- it became a variable length array sized from a `double`. Recorded at
/// #C124.
#[test]
fn diagnostics_floating_literal_is_not_an_integer_constant() {
    for (name, src, message) in [
        (
            "array_bound_file_scope",
            "int a[1.5];\nint main(void){ return 0; }\n",
            "size of array has non-integer type",
        ),
        (
            "array_bound_block_scope",
            "int main(void){ int a[1.5]; return sizeof a; }\n",
            "size of array has non-integer type",
        ),
        (
            "enumerator",
            "enum E { X = 1.5 };\nint main(void){ return 0; }\n",
            "constant",
        ),
        (
            "bitfield_width",
            "struct S { int b : 1.5; };\nint main(void){ return 0; }\n",
            "constant",
        ),
        (
            "static_assert",
            "_Static_assert(1.5, \"\");\nint main(void){ return 0; }\n",
            "constant",
        ),
    ] {
        compile_expect_error(name, src, message);
    }
}

/// What 6.6 *does* let a floating constant do, and what the two folders had to
/// agree on before they became one.
#[test]
fn diagnostics_shared_constant_folder_answers_alike() {
    assert_eq!(
        compile_and_run(
            "shared_constant_folder",
            "struct S { int x; int y; };\n\
             const int c = 5;\n\
             int arr[10];\n\
             int *p = &arr[c - 3];\n\
             int w = c + 1;\n\
             enum E { A = 3 };\n\
             int main(void) {\n\
             /* a cast of a floating constant, folded in floating point */\n\
             int cast_fold[(int)(1.5 + 1.5)];\n\
             /* a comparison of floating operands is an integer constant */\n\
             int cmp[1.5 > 1.0 ? 4 : 8];\n\
             /* _Alignof, which only one of the two folders used to know */\n\
             int aligned[_Alignof(double)];\n\
             /* the pre-<stddef.h> offsetof idiom */\n\
             int off[(int)(unsigned long)&((struct S *)0)->y];\n\
             _Static_assert(1.5 > 1.0, \"\");\n\
             _Static_assert((unsigned)-1 > 0, \"\");\n\
             if (sizeof cast_fold != 12) return 1;\n\
             if (sizeof cmp != 16) return 2;\n\
             if (sizeof aligned != 32) return 3;\n\
             if (sizeof off != 16) return 4;\n\
             if (w != 6) return 5;\n\
             if (p != &arr[2]) return 6;\n\
             if (A != 3) return 7;\n\
             if (!__builtin_constant_p(3.14)) return 8;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// Every operation is evaluated *in* a type, and its result narrowed to that
/// type before the next one sees it.
///
/// The folder carried a full-width `i128` instead, so `-1u / 3u` was 0 rather
/// than 1431655765 -- the negative was still negative when the division saw
/// it -- `(unsigned)-1 % 7u` was 4294967295 rather than 3, and a cast did not
/// convert at all: `(unsigned char)-1` was -1, `(short)70000` was 70000. Each
/// value below is gcc's. Recorded at #C126.
/// The folder has to read a constant at the operand's width, in the
/// signedness the *opcode* implies. An `i128` in the IR holds whatever bit
/// pattern the front end built -- `(int)0xFFFFFFFFu` emits no instruction at
/// all, so the constant still reads 4294967295 while every consumer now
/// treats it as a signed `int`.
///
/// Division, remainder and the ordering comparisons are the operations that
/// notice: unlike add/sub/mul they are not congruent modulo 2^n, so they read
/// the whole value and its sign. Everything here is correct at `-O0` and was
/// wrong at `-O`, which is what localizes it to the folder.
#[test]
fn diagnostics_signed_constants_fold_at_their_own_width() {
    let code = "int main(void) {\n\
         /* signed division and remainder of a same-width cast constant */\n\
         if ((int)0xFFFFFFFFu / 2 != 0) return 1;\n\
         if ((int)0xFFFFFFFFu % 3 != -1) return 2;\n\
         if ((int)0x80000000u / 2 != -1073741824) return 3;\n\
         if ((int)0x80000000u % 7 != -2) return 4;\n\
         /* the same value, spelled as a negative literal, must agree */\n\
         if ((-1) / 2 != 0) return 5;\n\
         if ((-1) % 3 != -1) return 6;\n\
         /* signed ordering comparisons */\n\
         if (!((int)0xFFFFFFFFu < 0)) return 7;\n\
         if (!((int)0xFFFFFFFFu <= -1)) return 8;\n\
         if (!((int)0xFFFFFFFFu == -1)) return 9;\n\
         if ((int)0xFFFFFFFFu > 0) return 10;\n\
         if ((int)0xFFFFFFFFu >= 0) return 11;\n\
         /* unsigned operators on the same bits must stay unsigned */\n\
         if (0xFFFFFFFFu / 2u != 2147483647u) return 12;\n\
         if (0xFFFFFFFFu % 3u != 0u) return 13;\n\
         if (0xFFFFFFFFu < 1u) return 14;\n\
         if (!(0xFFFFFFFFu > 1u)) return 15;\n\
         /* 64-bit, where the narrowing is not to 32 */\n\
         if ((long long)0xFFFFFFFFFFFFFFFFull / 2 != 0) return 16;\n\
         if ((long long)0xFFFFFFFFFFFFFFFFull < 0 ? 0 : 1) return 17;\n\
         /* narrower operands promote to int before dividing */\n\
         if ((signed char)-1 / 2 != 0) return 18;\n\
         if ((short)-1 % 3 != -1) return 19;\n\
         return 0;\n\
         }\n";
    assert_eq!(
        compile_and_run("signed_constants_fold_at_their_own_width", code, &[]),
        0
    );
}

#[test]
fn diagnostics_constants_fold_at_their_own_width() {
    assert_eq!(
        compile_and_run(
            "constants_fold_at_their_own_width",
            "typedef unsigned __int128 u128;\n\
             int main(void) {\n\
             /* division and remainder see an unsigned operand as unsigned */\n\
             if (-1u / 3u != 1431655765u) return 1;\n\
             if ((unsigned)-1 % 7u != 3u) return 2;\n\
             if ((0u - 1u) / 2u != 2147483647u) return 3;\n\
             if (-1ull / 3ull != 6148914691236517205ull) return 4;\n\
             /* a right shift of an unsigned value is logical */\n\
             if ((-1u >> 1) != 2147483647u) return 5;\n\
             if ((-1ull >> 1) != 9223372036854775807ull) return 6;\n\
             if ((~0u >> 28) != 15u) return 7;\n\
             /* 128 bits, where narrowing cannot help and signedness must */\n\
             u128 thirds = ((u128)6148914691236517205ull << 64) | 6148914691236517205ull;\n\
             if ((u128)-1 / 3 != thirds) return 8;\n\
             if ((long long)((u128)-1 >> 1) != -1) return 9;\n\
             /* a cast converts */\n\
             if ((int)(unsigned char)-1 != 255) return 10;\n\
             if ((int)(unsigned char)300 != 44) return 11;\n\
             if ((int)(short)70000 != 4464) return 12;\n\
             if ((int)(signed char)200 != -56) return 13;\n\
             if ((int)(unsigned short)-1 != 65535) return 14;\n\
             /* ... and a conversion to _Bool gives 0 or 1, not the low byte */\n\
             if ((int)(_Bool)2 != 1) return 15;\n\
             if ((int)(_Bool)256 != 1) return 16;\n\
             /* integer promotion still widens: these are int arithmetic */\n\
             if ((char)100 + (char)100 != 200) return 17;\n\
             if ((short)30000 + (short)30000 != 60000) return 18;\n\
             if ((unsigned char)200 + (unsigned char)200 != 400) return 19;\n\
             /* and signed arithmetic wraps at its own width */\n\
             if (4294967295u + 1u != 0u) return 20;\n\
             if ((unsigned)(1u << 31) != 2147483648u) return 21;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );

    // The same expressions as *constant* contexts, where only the folder can
    // answer: a run-time path that happened to be right would hide the bug.
    assert_eq!(
        compile_and_run(
            "constant_contexts_fold_alike",
            "enum E {\n\
             A = -1u / 3u,\n\
             B = (unsigned)-1 % 7u,\n\
             C = (int)(unsigned char)300,\n\
             D = (int)(short)70000,\n\
             F = (int)(_Bool)2,\n\
             G = (int)(-1u >> 1)\n\
             };\n\
             static unsigned s_div = -1u / 3u;\n\
             static int s_cast = (int)(unsigned char)300;\n\
             int main(void) {\n\
             int a[(int)(unsigned char)300];\n\
             if (A != 1431655765) return 1;\n\
             if (B != 3) return 2;\n\
             if (C != 44) return 3;\n\
             if (D != 4464) return 4;\n\
             if (F != 1) return 5;\n\
             if (G != 2147483647) return 6;\n\
             if (s_div != 1431655765u) return 7;\n\
             if (s_cast != 44) return 8;\n\
             if (sizeof a != 44 * sizeof(int)) return 9;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// 6.4.4.1p5 picks the first type that can represent the value, for a
/// `u`-suffixed constant as much as an unsuffixed one.
///
/// Every `u` suffix took `unsigned int` regardless of magnitude, so
/// `0xaaaaaaaaaaaaaaabu` had a four-byte type. That was survivable while
/// constants were carried at full width and only `sizeof` was wrong; once they
/// folded at their own width it truncated the value, and CPython's
/// `math.comb(5, 2)` returned 85899345930. Recorded at #C127.
#[test]
fn diagnostics_unsigned_suffix_widens_by_magnitude() {
    assert_eq!(
        compile_and_run(
            "unsigned_suffix_widens_by_magnitude",
            "int main(void) {\n\
             if (sizeof 1u != 4) return 1;\n\
             if (sizeof 0xFFFFFFFFu != 4) return 2;\n\
             if (sizeof 0x100000000u != 8) return 3;\n\
             if (sizeof 4294967296u != 8) return 4;\n\
             if (sizeof 0xaaaaaaaaaaaaaaabu != 8) return 5;\n\
             if (sizeof 18446744073709551615u != 8) return 6;\n\
             if (0xaaaaaaaaaaaaaaabu != 12297829382473034411ULL) return 7;\n\
             if (0x100000000u != 4294967296ULL) return 8;\n\
             /* the value that made math.comb wrong: a 64-bit product whose\n\
                left operand had been truncated to 32 bits */\n\
             if (0xfu * 0xaaaaaaaaaaaaaaabu != 5) return 9;\n\
             /* unsuffixed and l-suffixed spellings were already right */\n\
             if (sizeof 0xaaaaaaaaaaaaaaab != 8) return 10;\n\
             if (sizeof 1ul != 8) return 11;\n\
             if (sizeof 1ull != 8) return 12;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );

    // Through a static table, which is how CPython hit it: the initializer is
    // emitted from the folded value, so a truncated literal reaches .rodata.
    assert_eq!(
        compile_and_run(
            "wide_unsigned_literal_in_a_static_table",
            "static const unsigned long t[] = { 0xfu, 0xaaaaaaaaaaaaaaabu };\n\
             int main(void) {\n\
             unsigned long a = t[0], b = t[1];\n\
             if (b != 12297829382473034411UL) return 1;\n\
             if (a * b != 5) return 2;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

// ============================================================================
// #C116 — the lock-free atomic ceiling
// ============================================================================

/// An `_Atomic` object c17 cannot access lock-free falls back to an ordinary,
/// non-atomic access, and must say so.
///
/// Nothing in the suite asserted this text, so the ceiling could have moved --
/// in either direction -- without a test noticing. It is load-bearing: gcc
/// emits `__atomic_*` calls above it, which need `-latomic`, and c17 hands the
/// link to the host `cc` without it (#X1).
///
/// The 3-byte struct is the case worth naming. It is *under* eight bytes and
/// still not lock-free, because the hardware has no 3-byte atomic -- so the
/// rule is "at a machine width", not "small enough".
#[test]
fn diagnostics_non_lock_free_atomic_warns() {
    for (name, src) in [
        (
            "atomic_oversized_struct",
            "struct Big { int a, b, c; };\n_Atomic struct Big g;\nvoid f(struct Big v) { g = v; }\n",
        ),
        (
            "atomic_odd_width_struct",
            "struct Odd { char a, b, c; };\n_Atomic struct Odd g;\nvoid f(struct Odd v) { g = v; }\n",
        ),
        (
            "atomic_long_double",
            "_Atomic long double g;\nvoid f(long double v) { g = v; }\n",
        ),
    ] {
        compile_expect_warning(name, src, "is not atomic");
    }
}

/// The other side of the same rule: an aggregate *at* a lock-free width must
/// draw no diagnostic at all, because it is now genuinely atomic (#C116).
///
/// Without this, the test above passes just as well against a compiler that
/// warns on every `_Atomic` aggregate -- which is what c17 used to do.
#[test]
fn diagnostics_lock_free_atomic_aggregate_is_silent() {
    let src = r#"
struct S1 { char a; };
struct S2 { short a; };
struct S4 { int a; };
struct S8 { int a, b; };
union  U4 { int i; float f; };
_Atomic struct S1 g1;
_Atomic struct S2 g2;
_Atomic struct S4 g4;
_Atomic struct S8 g8;
_Atomic union  U4 gu;
void f(struct S1 a, struct S2 b, struct S4 c, struct S8 d, union U4 e) {
    g1 = a; g2 = b; g4 = c; g8 = d; gu = e;
}
"#;
    let c = create_c_file("atomic_lock_free_aggregate_silent", src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
    assert!(run.success, "should compile: {}", run.stderr);
    assert!(
        !run.stderr.contains("is not atomic"),
        "an aggregate at a lock-free width must not warn, got:\n{}",
        run.stderr
    );
}

/// A floating constant has no address, so a memory-class inline-asm
/// constraint cannot be satisfied. gcc says "memory input 0 is not directly
/// addressable" and stops; c17 reached `loc_to_asm_string` and panicked.
///
/// This is the one inline-asm constraint that has to be *rejected* rather than
/// materialized, and it is diagnosed in the backend -- the operand's actual
/// location is only known once registers are allocated -- so it also pins the
/// post-codegen error checkpoint that makes a backend diagnostic fail the
/// compile instead of writing the object anyway.
#[test]
fn diagnostics_float_constant_cannot_satisfy_a_memory_asm_constraint() {
    let src = r#"
int main(void) { __asm__ ("nop" :: "m"(1.0)); return 0; }
"#;
    compile_expect_error(
        "asm_float_const_memory_constraint",
        src,
        "not directly addressable",
    );
}

/// Every other constraint class accepts one: a general register takes the bit
/// pattern, an immediate substitutes it, and an SSE register gets it loaded.
/// Without this the test above would pass against a compiler that rejected
/// every floating asm operand.
#[cfg(target_arch = "x86_64")]
#[test]
fn diagnostics_float_constant_is_accepted_by_the_other_asm_classes() {
    for (name, constraint) in [
        ("asm_float_const_ok_r", "r"),
        ("asm_float_const_ok_i", "i"),
        ("asm_float_const_ok_g", "g"),
        ("asm_float_const_ok_x", "x"),
    ] {
        let src =
            format!("int main(void) {{ __asm__ (\"nop\" :: \"{constraint}\"(1.0)); return 0; }}\n");
        compile_expect_ok(name, &src);
    }
}

/// Only the reserved scratch registers are free across an asm body, and c17
/// has two of them (Xmm15 and Xmm14). A third SSE-class operand would have to
/// share one, silently overwriting a value, so it is refused instead.
///
/// The budget is shared by inputs and outputs: an `"=x"` output spends one,
/// leaving one for the inputs.
#[cfg(target_arch = "x86_64")]
#[test]
fn diagnostics_sse_asm_operands_are_limited_to_the_scratch_registers() {
    // Two fit.
    compile_expect_ok(
        "asm_two_sse_constants",
        "int main(void) { __asm__ (\"nop\" :: \"x\"(1.0), \"x\"(2.0)); return 0; }\n",
    );
    // A third does not.
    compile_expect_error(
        "asm_three_sse_constants",
        "int main(void) { __asm__ (\"nop\" :: \"x\"(1.0), \"x\"(2.0), \"x\"(3.0)); return 0; }\n",
        "too many SSE register constraints",
    );
    // An output spends one of the two, so two more inputs are one too many.
    compile_expect_error(
        "asm_sse_output_plus_two_inputs",
        "double r;\nint main(void) { __asm__ (\"nop\" : \"=x\"(r) : \"x\"(1.0), \"x\"(2.0)); return 0; }\n",
        "too many SSE register constraints",
    );
}

/// #C123: a constraint violation inside an abstract declarator must be
/// reported as itself, not discarded.
///
/// `try_parse_type_name_vm` backtracks by restoring the token cursor, and its
/// fallback arm collapsed two situations: "the declarator produced a name, so
/// this was never a type-name" and "this *is* a type-name and its declarator
/// is invalid". The second rewound too, so the real error was dropped and the
/// caller re-read `char[-1]` as a subscript expression -- producing
/// "undeclared identifier 'char'" and "subscripted value is neither array nor
/// pointer", two diagnostics about neither problem.
///
/// Every constraint an abstract declarator can violate was reported that way.
/// The wording is gcc's, including the named/unnamed distinction.
#[test]
fn diagnostics_abstract_declarator_reports_its_own_error() {
    for (name, src) in [
        (
            "abstract_neg_array_sizeof",
            "int main(void){ return sizeof(char[-1]); }\n",
        ),
        (
            "abstract_neg_array_alignof",
            "int main(void){ return _Alignof(char[-1]); }\n",
        ),
        (
            "abstract_neg_array_cast",
            "int main(void){ return (int)(char(*)[-1])0; }\n",
        ),
    ] {
        compile_expect_error(name, src, "size of unnamed array is negative");
    }

    // A declarator that *has* a name keeps naming it, as gcc does.
    compile_expect_error(
        "named_neg_array",
        "int main(void){ char a[-1]; return 0; }\n",
        "size of array 'a' is negative",
    );
}

/// The discarded diagnostic is only half the defect: the fallback's two
/// *spurious* messages were the visible half, and they must be gone.
#[test]
fn diagnostics_abstract_declarator_does_not_cascade() {
    let c = create_c_file(
        "abstract_no_cascade",
        "int main(void){ return sizeof(char[-1]); }\n",
    );
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);

    assert!(!run.success, "the program must still be rejected");
    for spurious in ["undeclared identifier", "subscripted value", "expected ')'"] {
        assert!(
            !run.stderr.contains(spurious),
            "the fallback parse still leaks {:?}:\n{}",
            spurious,
            run.stderr
        );
    }
    assert_eq!(
        run.stderr.matches("error:").count(),
        1,
        "one bad declarator must draw exactly one error:\n{}",
        run.stderr
    );
}

/// Committing to the type-name reading must not swallow the *expression*
/// reading, which is what the rewind is legitimately for: `(x)` in a cast
/// position is a parenthesized identifier, and `sizeof(x)` is sizeof an
/// object. Without these the test above would pass against a parser that had
/// simply stopped backtracking.
#[test]
fn diagnostics_type_name_backtracking_still_works() {
    compile_expect_ok(
        "backtrack_paren_expr",
        "int main(void){ int x = 3; return (x) - 3; }\n",
    );
    compile_expect_ok(
        "backtrack_sizeof_object",
        "int main(void){ int x = 0; (void)x; return sizeof(x) - sizeof(int); }\n",
    );
    compile_expect_ok(
        "backtrack_cast_to_ptr_to_array",
        "int a[3]; int main(void){ int (*p)[3] = (int(*)[3])&a; return (*p)[0]; }\n",
    );
    compile_expect_ok(
        "backtrack_compound_literal",
        "struct S { int a; };\nint main(void){ return ((struct S){0}).a; }\n",
    );
}

/// #C125: a shift whose constant count cannot name a bit of the value being
/// shifted draws a diagnostic, as gcc's does.
///
/// C17 6.5.7p3 makes such a shift undefined, and c17's answer -- the count
/// masked the way the hardware masks it -- is as defensible as gcc's, which is
/// not even self-consistent (`1 << 64` folds to 0 while `-1 >> 64` stays -1).
/// The gap was never the value; it was the silence.
///
/// The width is the *promoted left* operand's, so `(char)1 << 40` warns (char
/// promotes to int) and `1L << 63` does not. Only the count need be constant:
/// `x << 64` warns. Every row here was taken from `gcc -std=c17`.
#[test]
fn diagnostics_shift_count_out_of_range_warns() {
    for (name, src, expected) in [
        (
            "shift_left_64",
            "int main(void){ return 1 << 64; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_left_32",
            "int main(void){ return 1 << 32; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_right_64",
            "int main(void){ return 1 >> 64; }\n",
            "right shift count >= width of type",
        ),
        (
            "shift_long_64",
            "int main(void){ return (int)(1L << 64); }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_char_40",
            "int main(void){ return (char)1 << 40; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_var_left",
            "int x = 1;\nint main(void){ return x << 64; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_negative",
            "int main(void){ return 1 << -1; }\n",
            "left shift count is negative",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

/// The accept side, which is what keeps the check from being "warn on every
/// shift": a count inside the promoted left operand's width is silent, and so
/// is a count that is not a constant at all.
#[test]
fn diagnostics_shift_count_in_range_is_silent() {
    for (name, src) in [
        (
            "shift_left_31_ok",
            "int main(void){ return (1 << 31) != 0; }\n",
        ),
        (
            "shift_long_63_ok",
            "int main(void){ return (int)((1L << 63) != 0); }\n",
        ),
        ("shift_zero_ok", "int main(void){ return 1 << 0; }\n"),
        (
            "shift_var_count_ok",
            "int n = 3;\nint main(void){ return (1 << n) - 8; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// `-Wno-shift-count-overflow` and `-Wno-shift-count-negative` turn the two
/// groups off separately, as gcc spells them.
#[test]
fn diagnostics_shift_count_warnings_can_be_turned_off() {
    for (name, src, flag) in [
        (
            "shift_off_overflow",
            "int main(void){ return 1 << 64; }\n",
            "-Wno-shift-count-overflow",
        ),
        (
            "shift_off_negative",
            "int main(void){ return 1 << -1; }\n",
            "-Wno-shift-count-negative",
        ),
    ] {
        let c = create_c_file(name, src);
        let path = c.path().to_string_lossy().to_string();
        let run = run_c17(&[flag, "-S", "-o", "/dev/null", &path]);
        assert!(run.success, "{} should still compile: {}", name, run.stderr);
        assert!(
            !run.stderr.contains("shift count"),
            "{} did not silence the warning:\n{}",
            flag,
            run.stderr
        );
    }
}

/// C17 6.5.7p3: "the type of the result is that of the promoted left operand".
/// The right operand's type never reaches the result, and c17 took the usual
/// arithmetic conversions instead -- so `1 << 1L` came out `long` and
/// `sizeof(1 << 1L)` answered 8 where gcc answers 4. That width is also what
/// the warning above measures against, so the two had to be fixed together.
#[test]
fn diagnostics_shift_result_type_is_the_promoted_left_operand() {
    // Run it: compiling proves nothing here, since the wrong type compiles
    // just as cleanly as the right one.
    assert_eq!(
        compile_and_run(
            "shift_result_type",
            r#"
int main(void) {
    if (sizeof(1 << 1L) != sizeof(int)) return 1;
    if (sizeof(1L << 1) != sizeof(long)) return 2;
    if (sizeof((char)1 << 1) != sizeof(int)) return 3;
    if (sizeof(1U << 1L) != sizeof(unsigned int)) return 4;
    return 0;
}
"#,
            &[]
        ),
        0
    );
}

/// #C132: a diagnostic must name the type the source could have written.
///
/// `int m[4][8]; int *p = m;` reported the pointee as `int[8] *`, which reads
/// as "array of pointers" -- the other type entirely. `format_type` built the
/// spelling left to right and had no notion of a declarator's inside-out
/// reading, so it could not parenthesize. gcc says `int (*)[8]`.
#[test]
fn diagnostics_pointer_to_array_is_spelled_as_a_declarator() {
    let c = create_c_file("spell_ptr_to_array", "int m[4][8];\nint *p = m;\n");
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);

    assert!(
        run.stderr.contains("int (*)[8]"),
        "expected the declarator spelling gcc uses, got:\n{}",
        run.stderr
    );
    assert!(
        !run.stderr.contains("int[8] *"),
        "the suffix spelling names a different type:\n{}",
        run.stderr
    );
}

/// The composition, through a diagnostic rather than the type table directly:
/// a pointer to a function and an array of pointers must not collapse into
/// each other's spelling.
#[test]
fn diagnostics_function_pointer_is_spelled_as_a_declarator() {
    let c = create_c_file(
        "spell_fn_ptr",
        "int f(void);\nint (*fp)(void) = f;\nint bad = fp;\n",
    );
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);

    assert!(
        run.stderr.contains("int (*)(void)"),
        "expected `int (*)(void)`, got:\n{}",
        run.stderr
    );
}

/// Only one x87 asm output, because only one can be written back.
///
/// The write-back is a single slot; a second output would overwrite it and the
/// first result would be dropped, with the FP stack depth no longer matching
/// what the template left behind.
#[test]
#[cfg(target_arch = "x86_64")]
fn diagnostics_one_x87_asm_output() {
    compile_expect_error(
        "x87_two_outputs",
        "struct P { long double a, b; };\n\
         void g(struct P *p){ __asm__(\"fldz\\nfld1\" : \"=t\"(p->a), \"=u\"(p->b)); }\n\
         int main(void){ struct P p; g(&p); return 0; }\n",
        "x87 asm output",
    );
}

/// A `vector_size` beyond the maximum object size is refused.
///
/// This interns an array type directly, so nothing else would catch an absurd
/// width: `vector_size` once quietly produced a four-gigabyte type, and a
/// width near `u64::MAX` overflowed `next_power_of_two` -- a panic in a debug
/// build. The bound moved with the object-size limit; what this pins is that
/// the check is still reached.
#[test]
fn diagnostics_vector_size_is_bounded() {
    compile_expect_error(
        "vector_size_too_big",
        "typedef float V __attribute__((vector_size(9223372036854775808UL)));\n\
         int main(void){ return 0; }\n",
        "'vector_size' attribute argument value '9223372036854775808' exceeds \
         9223372036854775807",
    );
    compile_expect_error(
        "vector_size_negative",
        "typedef float V __attribute__((vector_size(-16)));\nint main(void){ return 0; }\n",
        "positive byte count",
    );
    compile_expect_error(
        "vector_size_not_multiple",
        "typedef double V __attribute__((vector_size(12)));\nint main(void){ return 0; }\n",
        "not a multiple",
    );
}

/// An attribute's integer argument is an integer constant expression, and
/// one that is not constant, or not a usable value, is refused in gcc's words
/// rather than dropped: the attribute parser once read a single token, so
/// `aligned(x)` and `aligned(3)` both left the object silently unaligned.
#[test]
fn diagnostics_attribute_integer_arguments() {
    let main = "int main(void){ return 0; }\n";
    for (name, decl, expected) in [
        (
            "aligned_not_constant",
            "int x; char a __attribute__((aligned(x)));",
            "requested alignment is not an integer constant",
        ),
        (
            "aligned_string",
            "char a __attribute__((aligned(\"s\")));",
            "requested alignment is not an integer constant",
        ),
        (
            "aligned_not_power_of_two",
            "char a __attribute__((aligned(3)));",
            "requested alignment '3' is not a positive power of 2",
        ),
        (
            "aligned_negative",
            "char a __attribute__((aligned(-4)));",
            "requested alignment '-4' is not a positive power of 2",
        ),
        (
            "aligned_too_large",
            "char a __attribute__((aligned(1ULL << 40)));",
            "exceeds object file maximum",
        ),
        (
            "aligned_two_arguments",
            "char a __attribute__((aligned(16, 32)));",
            "wrong number of arguments specified for 'aligned' attribute",
        ),
        (
            "alignas_not_constant",
            "int x; _Alignas(x) char a;",
            "requested alignment is not an integer constant",
        ),
        (
            "vector_size_not_constant",
            "int x; typedef int V __attribute__((vector_size(x)));",
            "'vector_size' attribute argument is not an integer constant",
        ),
        (
            "constructor_priority_not_constant",
            "int x; void f(void) __attribute__((constructor(x)));",
            "constructor priorities must be integers from 0 to 65535 inclusive",
        ),
        (
            "constructor_priority_out_of_range",
            "void f(void) __attribute__((constructor(70000)));",
            "constructor priorities must be integers from 0 to 65535 inclusive",
        ),
    ] {
        compile_expect_error(name, &format!("{decl}\n{main}"), expected);
    }
    // gcc warns about, and ignores, `aligned(0)`; and an attribute c17 does
    // not act on is dropped with a warning, not refused.
    compile_expect_warning(
        "aligned_zero",
        &format!("char a __attribute__((aligned(0)));\n{main}"),
        "requested alignment '0' is not a positive power of 2",
    );
    compile_expect_warning(
        "alloc_size_not_constant",
        &format!("int x; void *m(int) __attribute__((alloc_size(x)));\n{main}"),
        "'alloc_size' attribute argument is not an integer constant",
    );
    // Names stay names where the attribute wants one.
    compile_expect_ok(
        "attribute_names_and_strings",
        &format!(
            "enum {{ I = 1 }};\n\
             int pf(const char *, ...) __attribute__((__format__(__printf__, I, I + 1)));\n\
             typedef int SI __attribute__((mode(SI)));\n\
             char s __attribute__((section(\"a\" \"b\"), aligned));\n\
             void *m(int) __attribute__((alloc_size(I), malloc));\n{main}"
        ),
    );
}

/// Case ranges are checked for overlap, not just equality.
///
/// 6.8.4.2p3 forbids two equal case constants, and GCC extends that to
/// overlapping ranges. The check here was `Vec::contains` over individual
/// values; a range needs an interval test. Without it an overlapping arm
/// becomes silently unreachable, because the body walk resolves a label by
/// finding the first match.
#[test]
fn diagnostics_overlapping_case_ranges_are_rejected() {
    compile_expect_error(
        "case_range_overlap",
        "int f(int x){ switch(x){ case 1 ... 5: return 0; case 4 ... 9: return 1; } return 2; }\n\
         int main(void){ return f(0); }\n",
        "overlapping",
    );
    // A range that swallows a plain label is the same fault.
    compile_expect_error(
        "case_range_covers_single",
        "int f(int x){ switch(x){ case 1 ... 5: return 0; case 3: return 1; } return 2; }\n\
         int main(void){ return f(0); }\n",
        "case value",
    );
    // Adjacent ranges do not overlap and must be accepted.
    compile_expect_ok(
        "case_ranges_adjacent",
        "int f(int x){ switch(x){ case 1 ... 5: return 0; case 6 ... 9: return 1; } return 2; }\n\
         int main(void){ return f(0); }\n",
    );
}

/// An empty case range warns and never matches, as in GCC.
#[test]
fn diagnostics_empty_case_range_warns() {
    compile_expect_warning(
        "case_range_empty",
        "int f(int x){ switch(x){ case 9 ... 1: return 0; default: return 1; } }\n\
         int main(void){ return f(5); }\n",
        "empty range",
    );
}

/// Both endpoints of a range must be integer constant expressions.
#[test]
fn diagnostics_case_range_endpoints_must_be_constant() {
    compile_expect_error(
        "case_range_runtime_high",
        "int f(int x, int n){ switch(x){ case 1 ... n: return 0; } return 2; }\n\
         int main(void){ return f(0, 1); }\n",
        "constant expression",
    );
}

/// An array designator that addresses past the end of its array is rejected.
///
/// Nothing checked this anywhere: `int a[4] = {[10] = 7};` compiled and wrote
/// past the array, statically and at run time alike. GCC rejects it. Ranges
/// make it easy to write by accident, so the bound is checked where the array
/// size is known.
#[test]
fn diagnostics_designator_out_of_bounds() {
    compile_expect_error(
        "designator_past_end",
        "int a[4] = {[10] = 7};\nint main(void){ return a[0]; }\n",
        "exceeds array bounds",
    );
    compile_expect_error(
        "designator_range_past_end",
        "int a[4] = {[2 ... 9] = 7};\nint main(void){ return a[0]; }\n",
        "exceeds array bounds",
    );
    // An array sized *by* its initializer cannot overflow it.
    compile_expect_ok(
        "designator_infers_size",
        "int a[] = {[10] = 7};\nint main(void){ return a[10] == 7 ? 0 : 1; }\n",
    );
    // The last valid index is still valid.
    compile_expect_ok(
        "designator_last_index",
        "int a[4] = {[3] = 7};\nint main(void){ return a[3] == 7 ? 0 : 1; }\n",
    );
}

/// A reversed or negative index range is rejected, as in GCC.
#[test]
fn diagnostics_designator_range_is_well_formed() {
    compile_expect_error(
        "designator_range_reversed",
        "int a[4] = {[3 ... 1] = 5};\nint main(void){ return 0; }\n",
        "empty index range",
    );
    compile_expect_error(
        "designator_negative",
        "int a[4] = {[-1] = 5};\nint main(void){ return 0; }\n",
        "negative",
    );
    // A single-element range is well formed.
    compile_expect_ok(
        "designator_range_single",
        "int a[4] = {[1 ... 1] = 5};\nint main(void){ return a[1] == 5 ? 0 : 1; }\n",
    );
}

/// `&&label` naming a label the function never defines is an error.
///
/// The block minted for the reference stayed empty and unterminated, so
/// branching to it ran off the end of the function and the program hung.
/// Checked at the end of the function, because a forward reference is legal.
#[test]
fn diagnostics_label_address_must_name_a_label() {
    compile_expect_error(
        "label_addr_undefined",
        "int main(void){ void *p = &&nowhere; goto *p; return 1; }\n",
        "used but not defined",
    );
    // A forward reference is fine.
    compile_expect_ok(
        "label_addr_forward",
        "int main(void){ void *p = &&L; goto *p; return 1; L: return 0; }\n",
    );
}

/// Every way of naming a label -- `goto`, `&&label`, `asm goto` -- is checked
/// by one rule, wherever the reference sits. A `goto` inside a statement
/// expression escaped the check, which walked statements only, and ran off the
/// end of the function; one between case labels must still be caught.
#[test]
fn diagnostics_every_label_reference_must_name_a_label() {
    compile_expect_error(
        "goto_undefined_in_stmt_expr",
        "int f(void){ return ({ goto nowhere; 1; }); }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "goto_undefined_in_switch",
        "int f(int a){ switch (a) { case 0: goto nowhere; } return 0; }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "asm_goto_undefined",
        "int f(void){ asm goto(\"\" :::: nowhere); return 0; }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "label_addr_undefined_in_switch",
        "void *p;\nvoid f(int a){ switch (a) { case 0: p = &&nowhere; c1: a = 2; } }\n",
        "label 'nowhere' used but not defined",
    );
    // Labels in a switch body and in a statement expression are found by
    // both kinds of reference.
    compile_expect_ok(
        "labels_in_switch_and_stmt_expr",
        "void *p;\nint f(int a){\n  switch (a) { case 0: p = &&c1; goto c1; c1: a = 2; a1: case 1: a = 3; }\n  \
         return ({ p = &&se; goto se; se: ; a; });\n}\nint main(void){ return f(0) == 3 ? 0 : 1; }\n",
    );
}

/// `&&label` outside any function is an error, not a compiler crash.
#[test]
fn diagnostics_label_address_outside_a_function() {
    compile_expect_error(
        "label_addr_file_scope",
        "void *g = &&L;\nint main(void){ L: return 0; }\n",
        "outside of any function",
    );
}

/// The operand of a computed goto must be a pointer.
///
/// An integer is scalar, so testing scalarity accepted `goto *3;` — and the
/// 64-bit store of a 32-bit value then branched through a half-initialised
/// address.
#[test]
fn diagnostics_computed_goto_requires_a_pointer() {
    compile_expect_error(
        "computed_goto_int",
        "int main(void){ int n = 3; goto *n; return 1; }\n",
        "must be a pointer",
    );
    compile_expect_error(
        "computed_goto_double",
        "int main(void){ double d = 1.0; goto *d; return 1; }\n",
        "must be a pointer",
    );
}

/// An index range after a field designator is refused, not silently dropped.
///
/// `.m[0 ... 3] = v` resolves through the designator chain, which yields one
/// offset where a range names many, so it initialized nothing at all and said
/// nothing about it. The nested spelling does the same job.
#[test]
fn diagnostics_index_range_after_field_designator() {
    compile_expect_error(
        "range_after_field",
        "struct S { int m[4]; int t; };\nstruct S s = { .m[0 ... 3] = 7, .t = 9 };\n\
         int main(void){ return s.m[0]; }\n",
        "index range is not supported after a field designator",
    );
    // The nested form works and is what the diagnostic points at.
    compile_expect_ok(
        "range_nested_in_field",
        "struct S { int m[4]; int t; };\nstruct S s = { .m = { [0 ... 3] = 7 }, .t = 9 };\n\
         int main(void){ return (s.m[3] == 7 && s.t == 9) ? 0 : 1; }\n",
    );
}

// ============================================================================
// A declaration that declares nothing (C17 6.7p2)
// ============================================================================

/// 6.7p2 wants a declarator, a tag, or enumeration members. These have none.
///
/// All of them were **accepted silently** except the bare-specifier forms,
/// which drew "type specifier missing" -- blaming the half that was absent
/// rather than the declarator that was. gcc errors on `register`/`inline` here
/// and warns on the rest; c17 errors on all of them, which 6.7p2 permits since
/// it asks only for a diagnostic.
#[test]
fn diagnostics_declaration_that_declares_nothing_is_rejected() {
    // A storage class or qualifier is named, the way gcc names it.
    for (name, src, spec) in [
        ("declnothing_register", "int register;\n", "register"),
        ("declnothing_inline", "int inline;\n", "inline"),
        ("declnothing_static", "static;\n", "static"),
        ("declnothing_extern", "extern;\n", "extern"),
        ("declnothing_typedef", "int typedef;\n", "typedef"),
        ("declnothing_const", "const;\n", "const"),
        ("declnothing_volatile", "volatile;\n", "volatile"),
    ] {
        compile_expect_error(name, src, &format!("'{spec}' in empty declaration"));
    }

    // Nothing to name: just a type that declares no object.
    compile_expect_error("declnothing_int", "int;\n", "declaration declares nothing");
    compile_expect_error(
        "declnothing_unsigned",
        "unsigned;\n",
        "declaration declares nothing",
    );

    // Block scope has the same rule and its own parse path.
    compile_expect_error(
        "declnothing_block_static",
        "void f(void){ static; }\n",
        "'static' in empty declaration",
    );
    compile_expect_error(
        "declnothing_block_register",
        "void f(void){ int register; }\n",
        "'register' in empty declaration",
    );
}

/// The accept side, which is the half that can silently break real source.
///
/// A tag *is* something declared, so the whole point of this declaration form
/// keeps working; and a stray `;` stays legal because any function-like macro
/// expanding to nothing produces one (CPython's `_Py_DECLARE_STR()`).
#[test]
fn diagnostics_declarations_that_do_declare_something_are_accepted() {
    for (name, src) in [
        (
            "declok_struct_fwd",
            "struct S;\nint main(void){return 0;}\n",
        ),
        ("declok_union_fwd", "union U;\nint main(void){return 0;}\n"),
        (
            "declok_enum_def",
            "enum E { A };\nint main(void){return A;}\n",
        ),
        (
            "declok_struct_def",
            "struct S { int a; };\nint main(void){ struct S s = {0}; return s.a; }\n",
        ),
        ("declok_stray_semi", ";\nint main(void){return 0;}\n"),
        ("declok_object", "int x;\nint main(void){return x;}\n"),
        (
            "declok_static_object",
            "static int x;\nint main(void){return x;}\n",
        ),
        (
            "declok_typedef",
            "typedef int T;\nint main(void){ T t = 0; return t; }\n",
        ),
        (
            "declok_extern_object",
            "extern int x;\nint main(void){return 0;}\n",
        ),
        (
            "declok_register_local",
            "int main(void){ register int x = 0; return x; }\n",
        ),
        (
            "declok_inline_fn",
            "inline int f(void){return 0;}\nint main(void){return 0;}\n",
        ),
        (
            "declok_thread_local",
            "_Thread_local int x;\nint main(void){return x;}\n",
        ),
        (
            "declok_block_struct",
            "int main(void){ struct S; return 0; }\n",
        ),
        ("declok_block_semi", "int main(void){ ; return 0; }\n"),
        // A tag declared *with* a storage class still declares the tag.
        (
            "declok_typedef_struct",
            "typedef struct S T;\nint main(void){return 0;}\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// The implicit-int diagnostic must survive: it belongs to declarations that
/// *do* declare a declarator, which is the case this change routes around.
#[test]
fn diagnostics_implicit_int_still_outranks_the_empty_case() {
    compile_expect_error("stillint_global", "static x;\n", "type specifier missing");
    compile_expect_error(
        "stillint_local",
        "int main(void){ const y = 3; return y-3; }\n",
        "type specifier missing",
    );
}

// ============================================================================
// C11 6.7.5p2 — _Alignas is forbidden on a typedef, bit-field, function,
// parameter, or register object
// ============================================================================

/// C11 6.7.5p2 names five declarations an alignment specifier may not appear
/// in. None of the five was diagnosed; all five compiled silently.
///
/// The GNU `__attribute__((aligned(N)))` spelling is *not* covered by this
/// constraint — it is legal on a typedef and is how a typedef gets an
/// alignment at all — so only the `_Alignas` keyword is rejected here.
#[test]
fn diagnostics_alignas_forbidden_contexts_are_rejected() {
    compile_expect_error(
        "alignas_on_typedef",
        "_Alignas(64) typedef int T;\n",
        "_Alignas",
    );
    compile_expect_error(
        "alignas_on_bitfield",
        "struct S { _Alignas(64) int b : 3; };\n",
        "_Alignas",
    );
    compile_expect_error(
        "alignas_on_function",
        "_Alignas(64) void f(void);\n",
        "_Alignas",
    );
    compile_expect_error(
        "alignas_on_parameter",
        "void f(_Alignas(64) int p);\n",
        "_Alignas",
    );
    compile_expect_error(
        "alignas_on_register",
        "void f(void) { _Alignas(64) register int r; (void)r; }\n",
        "_Alignas",
    );
    // The same two, reached by the other declaration path.
    compile_expect_error(
        "alignas_on_function_block_scope",
        "void g(void) { _Alignas(64) void h(void); }\n",
        "_Alignas",
    );
    compile_expect_error(
        "alignas_on_param_of_function_pointer",
        "void (*fp)(_Alignas(64) int);\n",
        "_Alignas",
    );
}

/// The accept side, so the check above cannot pass by rejecting everything:
/// `_Alignas` on an ordinary object is the whole point of the feature, and the
/// `aligned` attribute stays legal everywhere it was.
#[test]
fn diagnostics_alignas_legal_contexts_still_compile() {
    compile_expect_ok("alignas_on_object", "_Alignas(64) int v;\n");
    compile_expect_ok("alignas_type_operand", "_Alignas(double) int v;\n");
    compile_expect_ok(
        "aligned_attr_on_typedef",
        "typedef int T __attribute__((aligned(64)));\nT v;\n",
    );
    compile_expect_ok(
        "aligned_attr_on_bitfield_struct",
        "struct S { int b : 3; } __attribute__((aligned(64)));\n",
    );
    compile_expect_ok(
        "alignas_on_struct_member",
        "struct S { _Alignas(64) int m; };\n",
    );
    // A pointer to function is an *object*, so it may carry an alignment even
    // though a function may not. The parameter list of such a declarator must
    // not see the enclosing `_Alignas` and mistake it for its own -- which is
    // exactly what the first attempt at the parameter check did.
    compile_expect_ok(
        "alignas_on_function_pointer_object",
        "_Alignas(64) void (*fp)(int);\n",
    );
    compile_expect_ok(
        "alignas_on_array_of_function_pointers",
        "_Alignas(64) void (*fps[4])(int);\n",
    );
    compile_expect_ok(
        "aligned_attr_on_function",
        "__attribute__((aligned(64))) void f(void) {}\n",
    );
}

/// A diagnostic names a complex type as the source wrote it.
///
/// `_Complex` is a `TypeModifiers` bit, not a `TypeKind`, and the type speller
/// printed only the kind -- so a `double _Complex` was reported as `double`:
/// a type the source never wrote, and one of a different size. The reader is
/// told the argument is a `double` and cannot see what is wrong with it.
#[test]
fn diag_complex_type_is_named_in_full() {
    compile_expect_error(
        "complex_type_named",
        "void f(int *p);\n\
         int main(void){ double _Complex z = 1.0; f(z); return 0; }\n",
        "double _Complex",
    );
}

// ============================================================================
// -fpermissive — two pre-C99 constructs, error by default
// ============================================================================

/// Compile `content` with the given extra flags and hand back the run.
fn compile_with(name: &str, content: &str, flags: &[&str]) -> crate::common::C17Run {
    let c_file = create_c_file(name, content);
    let path = c_file.path().to_string_lossy().to_string();
    let mut args: Vec<&str> = vec!["-S", "-o", "/dev/null"];
    args.extend_from_slice(flags);
    args.push(&path);
    run_c17(&args)
}

/// `-fpermissive` turns exactly two errors into warnings, and the default
/// must keep rejecting both.
///
/// The severity alone is not the assertion: a check that only looked at the
/// exit status would pass if the diagnostic vanished entirely, which is the
/// opposite of what is wanted. So the message text is asserted in both
/// directions -- still emitted, and emitted as a warning.
#[test]
fn diagnostics_fpermissive_downgrades_implicit_int() {
    let src = "static counter;\nf(x) int x; { return x; }\n";

    let strict = compile_with("permissive_off_int", src, &[]);
    assert!(!strict.success, "implicit int must be an error by default");
    assert!(
        strict.stderr.contains("error:") && strict.stderr.contains("type specifier missing"),
        "default build lost the implicit-int error:\n{}",
        strict.stderr
    );

    let lax = compile_with("permissive_on_int", src, &["-fpermissive"]);
    assert!(
        lax.success,
        "-fpermissive should accept implicit int:\n{}",
        lax.stderr
    );
    assert!(
        lax.stderr.contains("warning:") && lax.stderr.contains("type specifier missing"),
        "-fpermissive should still say something, as a warning:\n{}",
        lax.stderr
    );
    assert!(
        !lax.stderr.contains("error:"),
        "-fpermissive left an error behind:\n{}",
        lax.stderr
    );
}

#[test]
fn diagnostics_fpermissive_allows_implicit_function_declaration() {
    let src = "int main(void){ return undeclared_fn(1, 2); }\n";

    let strict = compile_with("permissive_off_fn", src, &[]);
    assert!(
        !strict.success && strict.stderr.contains("undeclared identifier"),
        "a call to an undeclared function must be an error by default:\n{}",
        strict.stderr
    );

    let lax = compile_with("permissive_on_fn", src, &["-fpermissive"]);
    assert!(
        lax.success,
        "-fpermissive should implicitly declare it:\n{}",
        lax.stderr
    );
    assert!(
        lax.stderr.contains("warning:")
            && lax.stderr.contains("implicit declaration of function")
            && lax.stderr.contains("undeclared_fn"),
        "-fpermissive should name the function it declared for you:\n{}",
        lax.stderr
    );
}

/// The implicit declaration is for a *call*. A bare undeclared identifier was
/// never implicitly declared by any C standard, and must stay an error even
/// under `-fpermissive` -- otherwise a misspelled variable silently becomes a
/// function and the program links against nothing.
#[test]
fn diagnostics_fpermissive_still_rejects_a_bare_undeclared_name() {
    for (name, src) in [
        (
            "perm_bare_name",
            "int main(void){ return mispelled_var; }\n",
        ),
        (
            "perm_bare_assign",
            "int main(void){ mispelled_var = 1; return 0; }\n",
        ),
        (
            "perm_bare_addr",
            "int main(void){ return *&mispelled_var; }\n",
        ),
    ] {
        let run = compile_with(name, src, &["-fpermissive"]);
        assert!(
            !run.success && run.stderr.contains("undeclared identifier"),
            "{name}: -fpermissive must not invent a variable:\n{}",
            run.stderr
        );
    }
}

/// `-fpermissive` relaxes those two constructs and nothing else: it is not a
/// dialect switch, and the rest of C17 still applies.
#[test]
fn diagnostics_fpermissive_is_not_a_dialect() {
    for (name, src, expected) in [
        (
            "perm_still_checks_args",
            "int f(int a, int b); int main(void){ return f(1); }\n",
            "argument",
        ),
        (
            "perm_still_checks_redecl",
            "int v; char v;\nint main(void){ return 0; }\n",
            "conflicting",
        ),
        (
            "perm_still_checks_assign_to_array",
            "int main(void){ int a[4], b[4]; a = b; return 0; }\n",
            "array type",
        ),
    ] {
        let run = compile_with(name, src, &["-fpermissive"]);
        assert!(
            !run.success && run.stderr.contains(expected),
            "{name}: -fpermissive should not have relaxed this:\n{}",
            run.stderr
        );
    }
}

/// An `always_inline` function that *cannot* be inlined must be diagnosed, not
/// left as a call to a symbol that was never emitted.
///
/// A C99 inline definition has no out-of-line copy, so refusing the attribute
/// silently produced `undefined reference` at link time -- a message naming
/// neither the attribute nor the reason. gcc rejects the same program
/// ("can never be inlined because it uses variable argument lists").
///
/// `va_start` is the refusal being exercised: it reads the enclosing
/// function's register save area, which no splice carries.
#[test]
fn diagnostics_always_inline_that_cannot_be_inlined_is_rejected() {
    let code = r#"
#include <stdarg.h>
long sink;
inline void __attribute__((always_inline)) bad(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    sink = va_arg(ap, long);
    va_end(ap);
}
int main(void) { bad(1, 42L); return 0; }
"#;
    compile_expect_error(
        "diag_always_inline_refused",
        code,
        "inlining failed in call to 'always_inline' 'bad'",
    );
}

/// The converse: an ordinary C99 inline definition is **not** an error, even
/// though it too has no out-of-line copy here. Its external definition may be
/// in another translation unit, which is exactly the idiom a header uses, so
/// an unsubstituted call is correct and the linker resolves it.
#[test]
fn diagnostics_plain_inline_definition_is_not_an_error() {
    let code = r#"
inline int helper(int a) { return a + 1; }
int use(int a) { return helper(a); }
int main(void) { return use(1) == 2 ? 0 : 1; }
"#;
    assert_eq!(compile_and_run("diag_plain_inline_ok", code, &[]), 0);
}

/// An `always_inline` call the inliner merely *declined* is not an error
/// either.
///
/// The caps on caller size and on recursive stack depth are c17's own -- gcc
/// has no counterpart -- so a refusal by one of them says nothing about
/// whether an out-of-line definition exists. Reporting it rejected programs
/// gcc compiles, and this is the shape that reaches it: glibc's
/// `__fortify_function` is exactly an `extern __inline` `gnu_inline`
/// `always_inline` definition, and any recursive function over a few hundred
/// instructions calling one declines the splice.
///
/// Two translation units, because that is the arrangement the idiom names: the
/// header's inline definition promises an out-of-line copy elsewhere, and the
/// call the inliner left standing is resolved against it.
#[test]
fn diagnostics_always_inline_declined_for_stack_depth_is_not_an_error() {
    let header_user = r#"
extern __inline __attribute__((__gnu_inline__, __always_inline__))
int helper(int x) { return x + 1; }

/* Large enough that the recursive-caller stack guard turns the splice down. */
int rec(int i)
{
    int t = 0;
    if (i <= 0) return 0;
    t += helper(i + 0);
    t += helper(i + 1);
    t += helper(i + 2);
    t += helper(i + 3);
    t += helper(i + 4);
    t += helper(i + 5);
    t += helper(i + 6);
    t += helper(i + 7);
    t += helper(i + 8);
    t += helper(i + 9);
    t += helper(i + 10);
    t += helper(i + 11);
    t += helper(i + 12);
    t += helper(i + 13);
    t += helper(i + 14);
    t += helper(i + 15);
    t += helper(i + 16);
    t += helper(i + 17);
    t += helper(i + 18);
    t += helper(i + 19);
    t += helper(i + 20);
    t += helper(i + 21);
    t += helper(i + 22);
    t += helper(i + 23);
    t += helper(i + 24);
    t += helper(i + 25);
    t += helper(i + 26);
    t += helper(i + 27);
    t += helper(i + 28);
    t += helper(i + 29);
    t += helper(i + 30);
    t += helper(i + 31);
    t += helper(i + 32);
    t += helper(i + 33);
    t += helper(i + 34);
    t += helper(i + 35);
    t += helper(i + 36);
    t += helper(i + 37);
    t += helper(i + 38);
    t += helper(i + 39);
    t += helper(i + 40);
    t += helper(i + 41);
    t += helper(i + 42);
    t += helper(i + 43);
    t += helper(i + 44);
    t += helper(i + 45);
    t += helper(i + 46);
    t += helper(i + 47);
    t += helper(i + 48);
    t += helper(i + 49);
    t += helper(i + 50);
    t += helper(i + 51);
    t += helper(i + 52);
    t += helper(i + 53);
    t += helper(i + 54);
    t += helper(i + 55);
    t += helper(i + 56);
    t += helper(i + 57);
    t += helper(i + 58);
    t += helper(i + 59);
    t += helper(i + 60);
    t += helper(i + 61);
    t += helper(i + 62);
    t += helper(i + 63);
    t += helper(i + 64);
    t += helper(i + 65);
    t += helper(i + 66);
    t += helper(i + 67);
    t += helper(i + 68);
    t += helper(i + 69);
    t += helper(i + 70);
    t += helper(i + 71);
    t += helper(i + 72);
    t += helper(i + 73);
    t += helper(i + 74);
    t += helper(i + 75);
    t += helper(i + 76);
    t += helper(i + 77);
    t += helper(i + 78);
    t += helper(i + 79);
    t += helper(i + 80);
    t += helper(i + 81);
    t += helper(i + 82);
    t += helper(i + 83);
    t += helper(i + 84);
    t += helper(i + 85);
    t += helper(i + 86);
    t += helper(i + 87);
    t += helper(i + 88);
    t += helper(i + 89);
    t += helper(i + 90);
    t += helper(i + 91);
    t += helper(i + 92);
    t += helper(i + 93);
    t += helper(i + 94);
    t += helper(i + 95);
    t += helper(i + 96);
    t += helper(i + 97);
    t += helper(i + 98);
    t += helper(i + 99);
    t += helper(i + 100);
    t += helper(i + 101);
    t += helper(i + 102);
    t += helper(i + 103);
    t += helper(i + 104);
    t += helper(i + 105);
    t += helper(i + 106);
    t += helper(i + 107);
    t += helper(i + 108);
    t += helper(i + 109);
    t += helper(i + 110);
    t += helper(i + 111);
    t += helper(i + 112);
    t += helper(i + 113);
    t += helper(i + 114);
    t += helper(i + 115);
    t += helper(i + 116);
    t += helper(i + 117);
    t += helper(i + 118);
    t += helper(i + 119);
    t += helper(i + 120);
    t += helper(i + 121);
    t += helper(i + 122);
    t += helper(i + 123);
    t += helper(i + 124);
    t += helper(i + 125);
    t += helper(i + 126);
    t += helper(i + 127);
    t += helper(i + 128);
    t += helper(i + 129);
    t += helper(i + 130);
    t += helper(i + 131);
    t += helper(i + 132);
    t += helper(i + 133);
    t += helper(i + 134);
    t += helper(i + 135);
    t += helper(i + 136);
    t += helper(i + 137);
    t += helper(i + 138);
    t += helper(i + 139);
    t += helper(i + 140);
    t += helper(i + 141);
    t += helper(i + 142);
    t += helper(i + 143);
    t += helper(i + 144);
    t += helper(i + 145);
    t += helper(i + 146);
    t += helper(i + 147);
    t += helper(i + 148);
    t += helper(i + 149);
    t += helper(i + 150);
    t += helper(i + 151);
    t += helper(i + 152);
    t += helper(i + 153);
    t += helper(i + 154);
    t += helper(i + 155);
    t += helper(i + 156);
    t += helper(i + 157);
    t += helper(i + 158);
    t += helper(i + 159);
    t += helper(i + 160);
    t += helper(i + 161);
    t += helper(i + 162);
    t += helper(i + 163);
    t += helper(i + 164);
    t += helper(i + 165);
    t += helper(i + 166);
    t += helper(i + 167);
    t += helper(i + 168);
    t += helper(i + 169);
    t += helper(i + 170);
    t += helper(i + 171);
    t += helper(i + 172);
    t += helper(i + 173);
    t += helper(i + 174);
    t += helper(i + 175);
    t += helper(i + 176);
    t += helper(i + 177);
    t += helper(i + 178);
    t += helper(i + 179);
    t += helper(i + 180);
    t += helper(i + 181);
    t += helper(i + 182);
    t += helper(i + 183);
    t += helper(i + 184);
    t += helper(i + 185);
    t += helper(i + 186);
    t += helper(i + 187);
    t += helper(i + 188);
    t += helper(i + 189);
    t += helper(i + 190);
    t += helper(i + 191);
    t += helper(i + 192);
    t += helper(i + 193);
    t += helper(i + 194);
    t += helper(i + 195);
    t += helper(i + 196);
    t += helper(i + 197);
    t += helper(i + 198);
    t += helper(i + 199);
    return t + rec(i - 1);
}

int main(void) { return rec(1) == 0 ? 1 : 0; }
"#;
    let out_of_line = r#"
int helper(int x) { return x + 1; }
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run_two_units(
                &format!("diag_always_inline_declined{opt}"),
                header_user,
                out_of_line,
                &[opt.to_string()],
            ),
            0,
            "at {opt}"
        );
    }
}

// ============================================================================
// What `-fpermissive` relaxes
// ============================================================================

/// The constraints gcc lets through, and c17 relaxes only when asked.
///
/// Each is a genuine C17 constraint violation, and each appears in source old
/// enough that gcc chose to warn rather than refuse. `-fpermissive` is where
/// c17 keeps that leniency: it already covers implicit `int` and implicit
/// function declarations, and these join them rather than becoming warnings
/// for everybody.
#[test]
fn diagnostics_permissive_relaxes_the_constraints_gcc_warns_about() {
    const CASES: &[(&str, &str, &str)] = &[
        (
            "return_without_value",
            "double g(void) { return; }\n",
            "'return' with no value",
        ),
        (
            "return_with_value",
            "void h(int v) { return v; }\n",
            "'return' with a value",
        ),
        (
            "struct_member_missing_semicolon",
            "struct S { int a; int b };\nint main(void){ return 0; }\n",
            "needs a ';'",
        ),
        (
            "inline_reads_a_file_scope_static",
            "static const int k = 3;\ninline int f(void) { return k; }\nint main(void){ return f() - 3; }\n",
            "cannot reference file-scope static",
        ),
        (
            "inline_updates_a_file_scope_static",
            "static int k;\ninline void f(void) { k += 3; }\nint main(void){ f(); return k - 3; }\n",
            "cannot reference file-scope static",
        ),
    ];

    for (name, src, needle) in CASES {
        // An error by default...
        compile_expect_error(name, src, "");
        // ...and a warning naming the same thing under -fpermissive.
        let warned =
            crate::common::compile_expect_warning_with(name, src, &["-fpermissive".to_string()]);
        assert!(
            warned.contains(needle),
            "{name}: -fpermissive should warn about {needle}, got:\n{warned}"
        );
    }
}

/// An object the *backend* cannot give a stack slot is diagnosed, not
/// miscompiled.
///
/// This bound is not C's. Both backends address a local and a stacked argument
/// by a signed 32-bit displacement from the frame register, so `i32::MAX`, less
/// the headroom the prologue adds, is the ceiling -- a billion times under the
/// `max_object_bytes` the type table allows, which is why the two are separate
/// bounds and separate messages. gcc compiles the same local with
/// `movabsq`-based 64-bit frame addressing; c17 says so instead, and refuses the
/// argument case exactly as gcc does ("sorry, unimplemented: passing too large
/// argument on stack").
///
/// There was no diagnostic at all before. `char a[3000000000];` in a function
/// emitted `subq $32, %rsp` with the array at `leaq -40(%rbp)` on x86-64 and a
/// 48-byte frame with it at `x29 + #40` on aarch64, because
/// `types.size_bytes(t) as i32` wrapped to -1294967296 and the `size.max(8)`
/// that follows gave it eight bytes.
///
/// The last three cases are not declarations, so `check_stack_object_size` is
/// asked of them somewhere other than the declarator loop: a compound literal
/// has automatic storage duration by C17 6.5.2.5p5, a K&R parameter's real type
/// arrives after the identifier list, and an aggregate *return* type is not an
/// object at all -- that one reaches `abi::slot_bytes` in the backend, which is
/// why its message differs.
#[test]
fn diagnostics_stack_object_larger_than_a_frame_slot_is_rejected() {
    for (name, src, expected) in [
        (
            "automatic_array",
            "extern void sink(char *);\n\
             int f(void){ char a[3000000000]; a[0]=1; sink(a); return a[0]; }\n",
            "maximum stack object size",
        ),
        (
            "automatic_array_of_int",
            "int f(void){ int a[600000000]; a[0]=1; return a[0]; }\n",
            "maximum stack object size",
        ),
        (
            "automatic_struct",
            "struct S { char x[3000000000]; };\n\
             int f(void){ struct S s; s.x[0]=1; return s.x[0]; }\n",
            "maximum stack object size",
        ),
        (
            "automatic_register",
            "int f(void){ register char a[3000000000]; return a[0]; }\n",
            "maximum stack object size",
        ),
        (
            "automatic_nested_block",
            "int f(int c){ if (c) { char a[3000000000]; return a[0]; } return 0; }\n",
            "maximum stack object size",
        ),
        (
            "parameter_prototype",
            "struct S { char x[3000000000]; };\nint f(struct S s);\n",
            "maximum stack object size",
        ),
        (
            "parameter_unnamed",
            "struct S { char x[3000000000]; };\nvoid f(struct S);\n",
            "maximum stack object size",
        ),
        (
            "parameter_definition",
            "struct S { char x[3000000000]; };\n\
             int f(struct S s){ return s.x[0]; }\n",
            "maximum stack object size",
        ),
        (
            "compound_literal",
            "struct S { char x[3000000000]; };\nvoid sink(struct S *);\n\
             void f(void){ sink(&(struct S){0}); }\n",
            "maximum stack object size",
        ),
        (
            "parameter_knr",
            "struct S { char x[3000000000]; };\n\
             int f(a) struct S a; { return a.x[0]; }\n",
            "maximum stack object size",
        ),
        (
            "aggregate_return_temporary",
            "struct S { char x[3000000000]; };\nextern struct S g(void);\n\
             int f(void){ return g().x[0]; }\n",
            "a stack frame slot can address",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// The same size at static storage duration, or behind a pointer, keeps
/// working.
///
/// The companion to the test above, and the guard that the new ceiling did not
/// become a second, tighter `max_object_bytes`: a static object is addressed
/// symbolically rather than from the frame, and `char g[3000000000];` emits
/// `.zero 3000000000` on both targets today.
///
/// `array_parameter_decays` and `pointer_to_a_large_struct` are the two cases
/// that break if the parameter check is moved *before* the C17 6.7.5.3
/// adjustment: that parameter is a `char *`, not an array. `vla_is_not_measured`
/// is the case the rule must decline to answer -- a variable length array's
/// extent is a run-time value, subtracted from the stack pointer in a 64-bit
/// register, and the frame holds only a pointer to it.
#[test]
fn diagnostics_static_object_larger_than_a_frame_slot_is_accepted() {
    for (name, src) in [
        (
            "file_scope_definition",
            "char big[3000000000];\nint main(void){ return big[0]; }\n",
        ),
        (
            "block_scope_static",
            "int f(void){ static char big[3000000000]; return big[0]; }\n",
        ),
        (
            "block_scope_extern",
            "int f(void){ extern char big[3000000000]; return big[0]; }\n",
        ),
        (
            "array_parameter_decays",
            "int f(char a[3000000000]){ return a[0]; }\n",
        ),
        (
            "pointer_to_a_large_struct",
            "struct S { char x[3000000000]; };\nint f(struct S *p){ return p->x[0]; }\n",
        ),
        (
            "sizeof_of_a_type_only",
            "struct S { char x[3000000000]; };\n\
             unsigned long f(void){ return sizeof(struct S); }\n",
        ),
        (
            "vla_is_not_measured",
            "int f(int n){ char a[n]; a[0]=1; return a[0]; }\n",
        ),
        // Deliberately modest. This case only has to show the check does not
        // fire on an ordinary automatic object; proving the *edge* of the
        // bound is `test_parser.rs`'s job, where it parses and never reaches a
        // backend. `compile_expect_ok` compiles for the **host**, whichever
        // backend that is, so an edge-sized local here tests nothing the
        // parser test does not and asks CI's machine for its size.
        (
            "automatic_object_of_an_ordinary_size",
            "extern void sink(char *);\n\
             int f(void){ char a[65536]; a[0]=1; sink(a); return a[0]; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A `vector_size` value where gcc gives it vector semantics is refused.
///
/// c17 implements a vector as storage only -- an array of its elements -- and
/// an array used as a value decays to its address. So each of these used to
/// compile to something other than what gcc means: `(long long)v` answered
/// the vector's address rather than its bits, `v + 1` did pointer
/// arithmetic, a vector argument or parameter went as a pointer. gcc's torture
/// tests `20050316-2`, `20050607-1` and `simd-4` all returned wrong answers.
#[test]
fn diagnostics_vector_value_is_refused() {
    let prelude = "typedef int V2SI __attribute__((vector_size(8)));\n\
                   long f(); long l; int c;\n";
    for (name, body) in [
        (
            "cast_from",
            "long t(void) { V2SI v = {1, 2}; return (long long)v; }",
        ),
        ("cast_to", "void t(void) { V2SI v = (V2SI)l; (void)&v; }"),
        (
            "binary",
            "void t(void) { V2SI v = {1, 2}; l = (long)(v + 1 == 0); }",
        ),
        ("unary", "void t(void) { V2SI v = {1, 2}; c = !v; }"),
        ("deref", "int t(void) { V2SI v = {1, 2}; return *v; }"),
        ("argument", "void t(void) { V2SI v = {1, 2}; f(v); }"),
        (
            "conditional",
            "void t(void) { V2SI v = {1, 2}; (void)(c ? v : v); }",
        ),
        ("parameter", "long t(V2SI v) { return 0; }"),
    ] {
        compile_expect_error(
            &format!("vector_value_{name}"),
            &format!("{prelude}{body}\n"),
            "'vector_size' types as storage only",
        );
    }
}

/// What the storage model gets right is still accepted: declaring a vector,
/// `sizeof`, `&v`, `v[i]`, a vector member, an initializer, and copying a
/// struct that holds one -- what glibc's `<link.h>` needs.
#[test]
fn diagnostics_vector_storage_is_accepted() {
    let src = r#"
typedef int V2SI __attribute__((vector_size(8)));
typedef float V4SF __attribute__((vector_size(16), aligned(16)));
struct regs { V4SF x[4]; long l; };
struct regs g;
int main(void)
{
    V2SI v = { 1, 2 };
    V2SI *p = &v;
    struct regs r = { 0 };
    r.x[1][2] = 3.0f;
    if (sizeof v != 8 || sizeof(struct regs) != 80) return 1;
    if (v[0] + (*p)[1] != 3) return 2;
    g = r;
    return g.x[1][2] == 3.0f ? 0 : 3;
}
"#;
    assert_eq!(compile_and_run("vector_storage", src, &[]), 0);
}

/// Naming a vector where its value is discarded is not a value use. `(void)v`
/// is how an unused variable is marked used, and it was refused: the cast
/// check did not tell a cast to `void` from a conversion. An expression
/// statement, the left operand of a comma, and the operands of `sizeof`,
/// `__alignof__` and `__typeof__` read nothing either.
#[test]
fn diagnostics_vector_discarded_value_is_accepted() {
    let src = r#"
typedef int V __attribute__((vector_size(8)));
int main(void)
{
    V v, w;
    (void)v;
    v;
    (v, 1);
    (void)sizeof v;
    (void)__alignof__(v);
    __typeof__(v) u;
    (void)u;
    v[0] = 3;
    w[1] = v[0];
    return w[1] == 3 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("vector_discarded", src, &[]), 0);
}

/// `__builtin_signbit` takes any real floating type, as gcc's does, and
/// refuses anything else as gcc does. A `long double` used to reach the
/// `double` emitter unconverted.
#[test]
fn diagnostics_signbit_is_type_generic() {
    let src = r#"
int main(void)
{
    volatile long double neg = -1.0L, pos = 1.0L, nz = -0.0L;
    volatile float f = -2.0f;
    volatile double d = -0.0;
    if (!__builtin_signbit(neg) || __builtin_signbit(pos) || !__builtin_signbit(nz)) return 1;
    if (!__builtin_signbit(f) || !__builtin_signbit(d)) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("signbit_generic", src, &[]), 0);
    let opts = vec!["-O2".to_string()];
    assert_eq!(compile_and_run("signbit_generic_o2", src, &opts), 0);
    compile_expect_error(
        "signbit_int",
        "int t(int x) { return __builtin_signbit(x); }\n",
        "non-floating-point argument",
    );
}

/// Frames at the edge of the ceiling, on both targets: diagnosed, never
/// wrapped.
///
/// Each of these was accepted before and came out wrong, because the ceiling
/// was `i32::MAX` and the arithmetic after it -- a slot's alignment rounding,
/// the prologue's saved registers and variadic save area, the final frame
/// rounding -- ran past it in `i32`:
///
/// - `_Alignas(16) char a[2147483640]`: the slot size was rounded up to its
///   alignment before the frame check, wrapped, and the array got zero bytes
///   inside a 16-byte frame on x86-64.
/// - a variadic function near the limit: the prologue total wrapped negative,
///   so x86-64 allocated no frame at all and addressed `2147483640(%rbp)`.
/// - a plain `char a[2147483632]` on aarch64: frame zeroing computed its last
///   store's offset in `i32`, wrapped into the unrolled path, and its loop never
///   ended -- the compiler pushed instructions until it ran out of memory.
/// - two by-value arguments of 1.5 GB each: the outgoing area's sum wrapped,
///   and x86-64 unrolled the copy into seven gigabytes of compiler memory.
#[test]
fn diagnostics_frame_at_the_ceiling_is_refused_not_wrapped() {
    let cases = [
        (
            "aligned_local",
            "extern void sink(void *);\n\
             void f(void){ _Alignas(16) char a[2147483640]; sink(a); }\n",
        ),
        (
            "variadic",
            "extern void sink(void *);\n\
             void f(int n, ...){ char a[2147483624]; sink(a); }\n",
        ),
        (
            "plain_local",
            "extern void sink(void *);\n\
             void f(void){ char a[2147483632]; sink(a); }\n",
        ),
        (
            "local_one_rounding_past",
            "extern void sink(void *);\n\
             void f(void){ _Alignas(64) char a[2147479480]; sink(a); }\n",
        ),
        (
            "stacked_arguments",
            "struct big { char b[1500000000]; };\n\
             extern void take(struct big, struct big);\n\
             void f(struct big *p){ take(*p, *p); }\n",
        ),
    ];
    for (name, src) in cases {
        for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
            let c = create_c_file(name, src);
            let out = c.path().with_extension("s");
            let run = run_c17(&[
                "--target",
                target,
                "-S",
                "-o",
                &out.to_string_lossy(),
                &c.path().to_string_lossy(),
            ]);
            let _ = std::fs::remove_file(&out);
            assert!(!run.success, "{name} on {target} compiled:\n{}", run.stderr);
            assert!(
                run.stderr.contains("stack object size")
                    || run.stderr.contains("stack frame")
                    || run.stderr.contains("stacked arguments"),
                "{name} on {target}: expected a frame diagnostic, got:\n{}",
                run.stderr
            );
        }
    }
}

/// Two tagless struct definitions are two types, even with the same members.
///
/// C17 6.7.2.3p5: each struct-or-union specifier with a member list declares a
/// distinct type. c17 compared tagless composites by their members alone, so
/// assigning one to the other -- or initializing one from the other -- was
/// accepted where gcc rejects it. Uses of *one* tagless type stay legal,
/// including through a typedef and a qualified variant of it.
#[test]
fn diagnostics_distinct_tagless_structs_are_incompatible() {
    compile_expect_error(
        "tagless_assign",
        "struct { long a, b; } x;\nstruct { long a, b; } y;\nvoid f(void){ x = y; }\n",
        "incompatible",
    );
    compile_expect_error(
        "tagless_init",
        "struct S { struct { long a, b; } pair; } *p;\n\
         long f(void){ struct { long a, b; } q = p->pair; return q.a; }\n",
        "",
    );
    compile_expect_ok(
        "tagless_same_type",
        "typedef struct { int x; } T;\nT t1;\nconst T t2;\nstruct { int x; } s1, s2;\n\
         struct o { struct { int y; } in; } a, b;\n\
         void f(void){ t1 = t2; s1 = s2; a.in = b.in; }\n",
    );
}

/// With several operands, an error inside a header names the translation unit
/// that included it. The note took its file name from the first stream ever
/// opened, so the second operand's error was reported against the first
/// operand: `one.c: note: in included file (through two.c)`.
#[test]
fn diagnostics_include_note_names_its_own_translation_unit() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_include_note_")
        .tempdir()
        .unwrap();
    let one = dir.path().join("one.c");
    let two = dir.path().join("two.c");
    std::fs::write(&one, "int main(void){return 0;}\n").unwrap();
    std::fs::write(&two, "#include \"bad.h\"\n").unwrap();
    std::fs::write(dir.path().join("bad.h"), "int x = undeclared_thing;\n").unwrap();
    let exe = dir.path().join("t.out");

    let r = run_c17(&[
        &one.to_string_lossy(),
        &two.to_string_lossy(),
        "-o",
        &exe.to_string_lossy(),
    ]);
    assert!(!r.success, "the header's error must fail the build");
    let note = r
        .stderr
        .lines()
        .find(|l| l.contains("in included file"))
        .unwrap_or_else(|| panic!("expected an include note:\n{}", r.stderr));
    assert!(
        note.starts_with(&*two.to_string_lossy()),
        "the note must name two.c, which included the header:\n{}",
        r.stderr
    );
}

/// `__attribute__((alias("target")))` needs its target *defined* in the same
/// unit, of the same kind, and the alias must not also be defined normally.
/// Each is a program gcc rejects; emitting it anyway gives an assembler error
/// at best and, for a second definition, silently drops one of the two.
// Mach-O has no symbol aliases, so c17 rejects `alias` on a Darwin host
// (`diagnostics_alias_attribute_unsupported_on_darwin` covers that side).
#[cfg(not(target_os = "macos"))]
#[test]
fn diagnostics_alias_attribute() {
    compile_expect_error(
        "alias_undefined",
        "extern int b __attribute__((alias(\"nope\")));\n",
        "'b' aliased to undefined symbol 'nope'",
    );
    // Declared is not defined: the target has to be in this unit.
    compile_expect_error(
        "alias_declared_only",
        "extern int x;\nextern int y __attribute__((alias(\"x\")));\n",
        "'y' aliased to undefined symbol 'x'",
    );
    compile_expect_error(
        "alias_to_inline_definition",
        "extern inline __attribute__((gnu_inline)) int f(void) { return 1; }\n\
         int g(void) __attribute__((alias(\"f\")));\n",
        "'g' aliased to external symbol 'f'",
    );
    compile_expect_error(
        "alias_object_to_function",
        "int f(void) { return 0; }\nextern int v __attribute__((alias(\"f\")));\n",
        "'v' alias between function and variable is not supported",
    );
    compile_expect_error(
        "alias_function_to_object",
        "int a;\nint g(void) __attribute__((alias(\"a\")));\n",
        "'g' alias between function and variable is not supported",
    );
    compile_expect_error(
        "alias_object_also_defined",
        "int a;\nextern int c __attribute__((alias(\"a\")));\nint c = 1;\n",
        "'c' defined both normally and as 'alias' attribute",
    );
    compile_expect_error(
        "alias_with_initializer",
        "int a;\nint b __attribute__((alias(\"a\"))) = 3;\n",
        "'b' defined both normally and as 'alias' attribute",
    );
    compile_expect_error(
        "alias_function_also_defined",
        "int f(void) { return 0; }\nint k(void) __attribute__((alias(\"f\")));\n\
         int k(void) { return 1; }\n",
        "'k' defined both normally and as 'alias' attribute",
    );
    compile_expect_ok(
        "alias_ok",
        "int a;\nextern int b __attribute__((alias(\"a\")));\n\
         int f(void) { return 0; }\nint g(void) __attribute__((alias(\"f\")));\n",
    );
}

/// Mach-O has no symbol aliases: clang rejects the attribute on Darwin, and
/// so does c17 rather than emit a `.set` whose symbol ld64 treats differently.
#[test]
fn diagnostics_alias_attribute_unsupported_on_darwin() {
    let src = "int f(void) { return 0; }\nint g(void) __attribute__((alias(\"f\")));\n";
    for target in ["aarch64-apple-darwin", "x86_64-apple-darwin"] {
        let c = create_c_file("alias_darwin", src);
        let out = c.path().with_extension("s");
        let run = run_c17(&[
            "--target",
            target,
            "-S",
            "-o",
            &out.to_string_lossy(),
            &c.path().to_string_lossy(),
        ]);
        let _ = std::fs::remove_file(&out);
        assert!(!run.success, "{target} accepted an alias:\n{}", run.stderr);
        assert!(
            run.stderr.contains("aliases are not supported on darwin"),
            "{target}: expected the Darwin diagnostic, got:\n{}",
            run.stderr
        );
    }
}

/// An error inside a struct or union specifier in a type-name is reported
/// where it arose. It was swallowed and the tokens re-read as an expression,
/// so compile/pr39394's VLA member in a cast drew "unexpected token in
/// expression" -- and, recovered too eagerly, a second error on the `*` in
/// front of the cast.
#[test]
fn diagnostics_struct_error_in_a_type_name_is_reported() {
    let src = "char *p;\n\
               void f(int n) {\n\
                   __asm__ volatile (\"\" : \"=m\" (*(struct { char x[n]; } *) p));\n\
               }\n\
               int g(int n) { return sizeof(union { int a[n]; }); }\n";
    let c = create_c_file("type_name_vla_member", src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
    assert!(!run.success, "should be rejected");
    let errors: Vec<&str> = run
        .stderr
        .lines()
        .filter(|l| l.contains("error:"))
        .collect();
    assert_eq!(
        errors.len(),
        2,
        "one error per type-name, got:\n{}",
        run.stderr
    );
    for e in errors {
        assert!(
            e.contains("variable length arrays cannot be structure or union members"),
            "{e}"
        );
    }
}

/// C17 6.8.4p3 and 6.8.5p5: a selection or iteration statement is a block,
/// and so is each substatement. A tag or enumeration constant declared in its
/// controlling expression, or in an expression statement that is its body,
/// leaked into the enclosing block.
#[test]
fn diagnostics_selection_and_iteration_statements_are_blocks() {
    for (name, src) in [
        (
            "if_scope",
            "int f(int c) { if (c == sizeof(enum { K = 3 })) return K; return K; }\n",
        ),
        (
            "while_body_scope",
            "int f(int c) { while (c--) (enum { L = 4 })0; return L; }\n",
        ),
        (
            "else_scope",
            "int f(int c) { if (c) (enum { M = 5 })0; else return M; return 0; }\n",
        ),
        (
            "switch_scope",
            "int f(int c) { switch (c == sizeof(enum { N = 1 })) { case 0: break; } return N; }\n",
        ),
        (
            "do_scope",
            "int f(int c) { do (enum { Q = 2 })0; while (c--); return Q; }\n",
        ),
    ] {
        compile_expect_error(name, src, "undeclared identifier");
    }
    compile_expect_error(
        "if_tag_scope",
        "int f(void) { if (sizeof(struct V { int a; })) {} struct V v; return 0; }\n",
        "not known",
    );
    // Inside the statement they are in scope.
    compile_expect_ok(
        "selection_scope_inside",
        "int f(int c) { if (c == sizeof(enum { K = 3 })) return K; \
         switch (c + sizeof(enum { N = 1 })) { case N: return N; } \
         for (int i = 0; i < sizeof(struct W { int a; }); i++) { struct W w = {i}; c += w.a; } \
         return c; }\n",
    );
}

// ============================================================================
// One declaration-specifier loop (C17 6.7.2, 6.7.7)
// ============================================================================

/// Compile `src` and return its stderr, requiring that it was rejected.
fn rejected_stderr(name: &str, src: &str, extra: &[&str]) -> String {
    let c = create_c_file(name, src);
    let path = c.path().to_string_lossy().to_string();
    let mut args = extra.to_vec();
    args.extend(["-S", "-o", "/dev/null", &path]);
    let run = run_c17(&args);
    assert!(!run.success, "'{name}' should have been rejected:\n{src}");
    run.stderr
}

/// A type-name's specifiers are the same specifier set a declaration's are
/// (C17 6.7.7p1), under the same 6.7.2p2 combination rules. The type-name
/// copy of the specifier loop kept no tally, so all of these compiled; gcc
/// rejects each with the wording asserted here.
#[test]
fn diagnostics_type_name_specifier_combinations_are_checked() {
    for (name, src, expected) in [
        (
            "tn_int_char",
            "int f(int x){ return (int char)x; }\n",
            "two or more data types in declaration specifiers",
        ),
        (
            "tn_float_double",
            "double f(double x){ return (float double)x; }\n",
            "two or more data types in declaration specifiers",
        ),
        (
            "tn_signed_unsigned",
            "int f(void){ return sizeof(signed unsigned); }\n",
            "both 'signed' and 'unsigned' in declaration specifiers",
        ),
        (
            "tn_signed_float",
            "float f(float x){ return (signed float)x; }\n",
            "both 'signed' and 'float' in declaration specifiers",
        ),
        (
            "tn_va_arg_int_char",
            "#include <stdarg.h>\nint f(int n, ...){ va_list ap; va_start(ap, n); \
             int r = va_arg(ap, int char); va_end(ap); return r; }\n",
            "two or more data types in declaration specifiers",
        ),
        (
            "tn_struct_int",
            "struct S { int a; };\nint f(void){ return sizeof(struct S int); }\n",
            "two or more data types in declaration specifiers",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
}

/// C17 6.7.2.4p3 and 6.7.3p3 forbid `_Atomic` on an array or function type
/// wherever the type is written. Only the declaration path checked; in a
/// type-name `_Atomic(int[3])` and `_Atomic A` (an array typedef) compiled.
#[test]
fn diagnostics_atomic_type_name_on_array_or_function_is_rejected() {
    compile_expect_error(
        "tn_atomic_array",
        "int f(void){ return sizeof(_Atomic(int[3])); }\n",
        "'_Atomic' cannot be applied to an array type",
    );
    compile_expect_error(
        "tn_atomic_typedef_array",
        "typedef int A[3];\nint f(void){ return sizeof(_Atomic A); }\n",
        "'_Atomic' cannot be applied to an array type",
    );
    compile_expect_error(
        "tn_atomic_typedef_function",
        "typedef int F(void);\nint f(void){ return sizeof(_Atomic F *); }\n",
        "'_Atomic' cannot be applied to a function type",
    );
}

/// The specifier arm and the wrapper around the loop each checked `_Atomic`,
/// so one `_Atomic(int[3])` drew the same error twice.
#[test]
fn diagnostics_atomic_array_is_reported_once() {
    for (name, src) in [
        ("atomic_array_once", "_Atomic(int[3]) v;\n"),
        ("atomic_member_once", "struct S { _Atomic(int[3]) v; };\n"),
        ("atomic_function_once", "_Atomic(int(void)) *w;\n"),
    ] {
        let stderr = rejected_stderr(name, src, &[]);
        assert_eq!(
            stderr.matches("'_Atomic' cannot be applied").count(),
            1,
            "{name}: one constraint violation, one diagnostic:\n{stderr}"
        );
    }
}

/// C17 6.7p1: the declaration specifiers may come in any order, so anything
/// may follow a complete `typeof(..)`, `_Atomic(..)`, enum, struct or union
/// specifier. gcc accepts all of these.
#[test]
fn diagnostics_specifiers_after_a_complete_type_specifier_are_accepted() {
    for (name, src) in [
        ("typeof_const", "typeof(int) const x = 1;\n"),
        ("atomic_const", "_Atomic(int) const y = 1;\n"),
        (
            "tn_atomic_const",
            "int f(void){ return sizeof(_Atomic(int) const); }\n",
        ),
        (
            "tn_typeof_const",
            "int f(void){ return sizeof(typeof(int) const); }\n",
        ),
        ("enum_static_file", "enum E { A } static e;\n"),
        (
            "enum_static_block",
            "void f(void){ enum E { A } static e; (void)e; }\n",
        ),
        (
            "struct_attr_static",
            "struct S { int a; } __attribute__((unused)) static s;\n",
        ),
        (
            "struct_const_attr_extern",
            "struct S { int a; } const __attribute__((unused)) extern s;\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A second data type after a tag specifier is the ordinary 6.7.2p2
/// violation. The struct and enum arms returned before the tally saw the
/// `int`, which was then read as the declarator and reported as a stray
/// identifier.
#[test]
fn diagnostics_data_type_after_tag_specifier_is_rejected() {
    for (name, src) in [
        ("struct_int", "struct S { int a; };\nstruct S int x;\n"),
        (
            "struct_int_block",
            "struct S { int a; };\nvoid f(void){ struct S int x; }\n",
        ),
        ("enum_int", "enum E { A };\nenum E int x;\n"),
    ] {
        compile_expect_error(
            name,
            src,
            "two or more data types in declaration specifiers",
        );
    }
}

/// `__float128` names a type only where the target has one. A declaration
/// said so; a type-name broke out of the loop and re-read the keyword as an
/// expression, reporting an undeclared identifier.
#[test]
fn diagnostics_float128_type_name_on_a_target_without_it() {
    let stderr = rejected_stderr(
        "tn_float128",
        "int f(void){ return sizeof(__float128); }\n",
        &["--target", "aarch64-apple-darwin"],
    );
    assert!(
        stderr.contains("__float128 is not supported on this target"),
        "{stderr}"
    );
    assert!(!stderr.contains("undeclared identifier"), "{stderr}");
}

/// A typedef name is a type specifier only when no type specifier of any kind
/// has been given (C17 6.7.2p2). `unsigned`, `long` and `_Complex` set no base
/// type, so `unsigned T x;` took `T` as the type where gcc -- and the
/// standard -- read it as the declarator; and after a typedef name a further
/// type specifier was silently ignored.
#[test]
fn diagnostics_typedef_name_combines_with_no_other_type_specifier() {
    compile_expect_error(
        "unsigned_T",
        "typedef int T;\nunsigned T x;\n",
        "found identifier 'x'",
    );
    compile_expect_error(
        "complex_T",
        "typedef double T;\n_Complex T x;\n",
        "found identifier 'x'",
    );
    compile_expect_error(
        "T_int",
        "typedef int T;\nT int x;\n",
        "two or more data types in declaration specifiers",
    );
    compile_expect_error(
        "tn_T_int",
        "typedef int T;\nint f(void){ return sizeof(T int); }\n",
        "two or more data types in declaration specifiers",
    );
    compile_expect_error(
        "T_unsigned",
        "typedef int T;\nT unsigned x;\n",
        "both 'unsigned' and 'T' in declaration specifiers",
    );
    for (name, src) in [
        (
            "tn_unsigned_T",
            "typedef int T;\nint f(void){ return sizeof(unsigned T); }\n",
        ),
        (
            "tn_complex_T",
            "typedef double T;\nint f(void){ return sizeof(_Complex T); }\n",
        ),
    ] {
        compile_expect_error(name, src, "expected ')' before 'T'");
    }
}

/// Once a type-name's first token has committed it, it is not re-read as an
/// expression. The type-name loop answered "not a type" after consuming
/// tokens, and its callers carried on from wherever it stopped: an
/// attribute-only `sizeof(__attribute__((unused)) y)` compiled, and
/// `sizeof(int x)` reported `int` as an undeclared identifier.
#[test]
fn diagnostics_committed_type_name_is_not_reparsed_as_an_expression() {
    let stderr = rejected_stderr(
        "tn_attr_only",
        "int y; int f(void){ return sizeof(__attribute__((unused)) y); }\n",
        &[],
    );
    assert!(stderr.contains("type specifier missing"), "{stderr}");
    assert!(stderr.contains("expected ')' before 'y'"), "{stderr}");

    let stderr = rejected_stderr("tn_named", "int f(void){ return sizeof(int x); }\n", &[]);
    assert!(stderr.contains("expected ')' before 'x'"), "{stderr}");
    assert!(!stderr.contains("undeclared identifier"), "{stderr}");

    // The expression readings the gate must leave alone.
    compile_expect_ok(
        "tn_expr_readings",
        "typedef int T; int x;\n\
         int f(void){ return sizeof(x) + (x) + sizeof x + sizeof(T) + (int)(T)x; }\n",
    );
}

/// A type-name must name a type: C17 6.7.2p2 requires a type specifier, and
/// c17 does not default to `int` (see `check_implicit_int`). gcc warns
/// "type defaults to 'int' in type name"; the type-name loop said nothing.
#[test]
fn diagnostics_type_name_without_type_specifier_is_rejected() {
    compile_expect_error(
        "tn_const_only",
        "int f(void){ return sizeof(const); }\n",
        "type specifier missing",
    );
    compile_expect_error(
        "tn_const_cast",
        "int f(int x){ return (const)x; }\n",
        "type specifier missing",
    );
    compile_expect_error(
        "member_const_only",
        "struct S { const x; };\n",
        "type specifier missing",
    );
}

/// A type-name and a member declaration take a specifier-qualifier list
/// (C17 6.7.2.1p1, 6.7.7p1): no storage class and no function specifier.
/// Members accepted them silently; in a type-name they fell out of the
/// specifier loop and were reported as undeclared identifiers. gcc's wording.
#[test]
fn diagnostics_specifier_qualifier_list_rejects_declaration_only_specifiers() {
    for (name, src, word) in [
        ("member_static", "struct S { static int x; };\n", "static"),
        ("member_extern", "struct S { extern int x; };\n", "extern"),
        (
            "member_typedef",
            "struct S { typedef int x; };\n",
            "typedef",
        ),
        ("member_inline", "struct S { inline int x; };\n", "inline"),
        (
            "member_noreturn",
            "struct S { _Noreturn int x; };\n",
            "_Noreturn",
        ),
        (
            "member_thread_local",
            "struct S { _Thread_local int x; };\n",
            "_Thread_local",
        ),
        (
            "tn_static_cast",
            "int f(int x){ return (static int)x; }\n",
            "static",
        ),
        (
            "tn_register_sizeof",
            "int f(void){ return sizeof(register int); }\n",
            "register",
        ),
        (
            "tn_typedef_sizeof",
            "int f(void){ return sizeof(typedef int); }\n",
            "typedef",
        ),
        (
            "tn_inline_sizeof",
            "int f(void){ return sizeof(inline int); }\n",
            "inline",
        ),
        (
            "tn_static_generic",
            "int x; int f(void){ return _Generic(x, static int: 1, default: 2); }\n",
            "static",
        ),
        // C23 admits a storage class in a compound literal, and gcc takes it
        // before C23 as an extension it flags under -pedantic. C17 does not.
        (
            "tn_static_compound",
            "int f(void){ return (static int){1}; }\n",
            "static",
        ),
    ] {
        let stderr = rejected_stderr(name, src, &[]);
        let expected = format!("expected specifier-qualifier-list before '{word}'");
        assert!(stderr.contains(&expected), "{name}: {stderr}");
        assert!(
            !stderr.contains("undeclared identifier"),
            "{name}: {stderr}"
        );
    }

    compile_expect_error(
        "tn_alignas",
        "int f(void){ return sizeof(_Alignas(8) int); }\n",
        "_Alignas cannot be applied to a type name",
    );
    // A member may carry an alignment specifier (C17 6.7.5p2 excludes only a
    // bit-field), and a member's qualifiers are what they always were.
    compile_expect_ok(
        "member_alignas",
        "struct S { _Alignas(8) int x; const volatile int y; };\n",
    );
}

/// A parameter declaration may carry no storage class but `register`
/// (C17 6.7.6.3p2). Every other one was accepted and ignored -- in a
/// prototype, a definition and a K&R declaration list alike.
#[test]
fn diagnostics_parameter_storage_class_other_than_register_is_rejected() {
    for (name, src) in [
        ("param_static", "void f(static int x);\n"),
        ("param_extern", "void f(extern int x);\n"),
        ("param_auto", "void f(auto int x);\n"),
        ("param_typedef", "void f(typedef int x);\n"),
        ("param_thread_local", "void f(_Thread_local int x);\n"),
        ("param_static_def", "int f(static int x){ return x; }\n"),
        ("knr_static", "int f(x) static int x; { return x; }\n"),
    ] {
        compile_expect_error(name, src, "storage class specified for parameter 'x'");
    }
    compile_expect_error(
        "param_unnamed_static",
        "void f(static int);\n",
        "storage class specified for unnamed parameter",
    );
    compile_expect_warning(
        "param_inline",
        "void f(inline int x);\n",
        "parameter 'x' declared 'inline'",
    );
    compile_expect_warning(
        "param_noreturn",
        "void f(_Noreturn int x);\n",
        "parameter 'x' declared '_Noreturn'",
    );
    for (name, src) in [
        ("param_register", "int f(register int x){ return x; }\n"),
        ("knr_register", "int f(x) register int x; { return x; }\n"),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A typedef name shares the ordinary name space with objects, functions and
/// enumerators (C17 6.2.3), so declaring one over the other in the same scope
/// is 6.7p3. Neither direction was checked: `typedef int T; T T;` and
/// `int x; typedef int x;` both compiled. Shadowing in an inner scope stays
/// legal.
#[test]
fn diagnostics_typedef_and_object_in_one_scope_collide() {
    for (name, src) in [
        ("td_then_object", "typedef int T;\nT T;\n"),
        (
            "td_then_object_block",
            "void f(void){ typedef int T; int T; }\n",
        ),
        ("object_then_td", "int x;\ntypedef int x;\n"),
        ("enumerator_then_td", "enum { E };\ntypedef int E;\n"),
        ("parameter_then_td", "void f(int x){ typedef int x; }\n"),
    ] {
        compile_expect_error(name, src, "redeclared as a different kind of symbol");
    }
    compile_expect_ok(
        "td_shadowed_in_inner_scope",
        "typedef int T;\nvoid f(void){ T T; T = 1; (void)T; }\nvoid g(int T){ (void)T; }\n",
    );
}

// ============================================================================
// One path binds a declarator, whichever position it holds
// ============================================================================
//
// A file-scope declaration had five hand-written binders -- the first plain
// declarator, a grouped one, a function declarator, the declarators after the
// first, and block scope's -- and each check lived in some of them. Every case
// below is a check one path made and another did not, so each is spelled in
// the positions that used to skip it.

/// C17 6.7.6.2p1: an array's element type must be complete where the array is
/// declared. Only the first plain file-scope declarator asked.
#[test]
fn diagnostics_incomplete_element_type_in_every_declarator_position() {
    for (name, src) in [
        ("inc_elem_later", "struct T;\nstruct T *p, arr[2];\n"),
        ("inc_elem_grouped", "struct T;\nstruct T (arr)[2];\n"),
        (
            "inc_elem_block_extern",
            "struct T;\nvoid f(void){ extern struct T arr[2]; }\n",
        ),
        (
            "inc_elem_block",
            "struct U;\nvoid f(void){ struct U a[2]; }\n",
        ),
        ("inc_elem_typedef", "struct T;\ntypedef struct T A[2];\n"),
        (
            "inc_elem_block_typedef",
            "struct T;\nvoid f(void){ typedef struct T A[2]; }\n",
        ),
    ] {
        compile_expect_error(name, src, "array type has incomplete element type");
    }
    compile_expect_ok(
        "inc_elem_completed_first",
        "struct T;\nstruct T *p;\nstruct T { int a; };\nstruct T arr[2], *q;\n\
         void f(void){ struct T b[2]; extern struct T c[2]; (void)b; }\n",
    );
}

/// An automatic array needs its extent where it is declared; nothing later in
/// the block can supply one. The block binder asked only whether a *tag* was
/// complete, which an array never is not.
#[test]
fn diagnostics_block_scope_array_without_a_size_is_rejected() {
    compile_expect_error(
        "block_unsized_array",
        "void f(void){ int b[]; (void)b; }\n",
        "array size missing in 'b'",
    );
    compile_expect_error(
        "block_unsized_array_later",
        "void f(void){ int a, b[]; (void)a; (void)b; }\n",
        "array size missing in 'b'",
    );
    compile_expect_ok(
        "block_sized_arrays",
        "void f(int n){ int a[] = {1, 2}; extern int e[]; typedef int T[]; \
         int v[n]; int (*p)[n]; (void)a; (void)v; (void)p; }\n",
    );
}

/// C17 6.7.6.2p2: a block-scope object of variably modified type may have no
/// linkage, and a variable length array may not have static storage. Both
/// compiled silently, the array with no storage at all -- however it came by
/// its extent: written, through a typedef, or through `typeof`.
#[test]
fn diagnostics_variably_modified_object_with_static_storage_or_linkage() {
    for (name, src) in [
        ("vm_static_array", "void f(int n){ static int s[n]; }\n"),
        (
            "vm_static_typedef",
            "void f(int n){ typedef int T[n]; static T s; }\n",
        ),
        (
            "vm_static_typeof",
            "void f(int n){ int v[n]; static typeof(v) s; }\n",
        ),
        (
            "vm_thread_local",
            "void f(int n){ static _Thread_local int s[n]; }\n",
        ),
    ] {
        compile_expect_error(name, src, "storage size of 's' isn't constant");
    }
    for (name, src) in [
        ("vm_extern_array", "void f(int n){ extern int e[n]; }\n"),
        (
            "vm_extern_typeof",
            "void f(int n){ int (*p)[n] = 0; extern typeof(p) e; }\n",
        ),
    ] {
        compile_expect_error(
            name,
            src,
            "object with variably modified type must have no linkage",
        );
    }
    // A pointer to a VLA is variably modified but no VLA, and static storage
    // allows it.
    compile_expect_ok(
        "vm_static_pointer",
        "int f(int n){ static int (*p)[n]; return (int)sizeof *p; }\n",
    );
}

/// The constraints on a type-name of variably modified or non-scalar type:
/// a compound literal may not be a VLA (C17 6.5.2.5p1), a cast names a scalar
/// type (6.5.4p2), and a `_Generic` association no variably modified type
/// (6.5.1.1p2). All three compiled silently -- `(int[n]){0}` as a one-element
/// array, a cast to an array as its first element's address.
#[test]
fn diagnostics_type_name_constraints_on_variably_modified_and_array_types() {
    for (name, src, expected) in [
        (
            "vla_compound_literal",
            "int f(int n){ return (int[n]){0}[0]; }\n",
            "compound literal has variable size",
        ),
        (
            "vla_compound_literal_alignof",
            "int f(int n){ return _Alignof((int[n]){0}); }\n",
            "compound literal has variable size",
        ),
        (
            "cast_to_vla",
            "int f(int n, int *p){ return sizeof((int[n])p); }\n",
            "cast specifies array type",
        ),
        (
            "cast_to_array",
            "int f(int *p){ return ((int[3])p)[0]; }\n",
            "cast specifies array type",
        ),
        (
            "cast_to_function",
            "int g(void); int f(void){ return ((int(void))g)(); }\n",
            "cast specifies function type",
        ),
        (
            "generic_vm_association",
            "int f(int n){ int (*a)[5] = 0; return _Generic(a, int (*)[n]: 1, default: 2); }\n",
            "'_Generic' association has variable length type",
        ),
    ] {
        compile_expect_error(name, src, expected);
    }
    // A pointer to a VLA may be a compound literal, and a union a cast.
    compile_expect_ok(
        "vm_type_names_accepted",
        "union U { int i; float f; };\n\
         int f(int n, void *p){ int (*q)[n] = (int (*)[n]){ p }; \
         return (int)sizeof *q + (int)((union U)1).i; }\n",
    );
}

/// C17 6.9.2p3: a tentative definition may be completed later in the unit, but
/// something must complete it. Only the first plain declarator was recorded
/// for the end-of-unit check, so the rest compiled with no storage at all.
#[test]
fn diagnostics_tentative_definition_never_completed_in_every_position() {
    for (name, src) in [
        ("tent_later", "struct U;\nint x;\nstruct U *p, u;\n"),
        ("tent_grouped", "struct U;\nstruct U (u);\n"),
    ] {
        compile_expect_error(name, src, "storage size of an object");
    }
    compile_expect_ok(
        "tent_later_completed",
        "struct U;\nstruct U *p, u, (w);\nstruct U { int a; };\n",
    );
}

/// C17 6.2.7p4: a later declaration of `extern int a[];` supplies its extent,
/// whichever position in its list it holds.
#[test]
fn diagnostics_extern_array_completed_by_any_declarator() {
    compile_expect_ok(
        "extern_completed_later",
        "extern int a[];\nint z, a[4];\n_Static_assert(sizeof a == 16, \"a\");\n",
    );
    compile_expect_ok(
        "extern_completed_grouped",
        "extern int b[];\nint (b)[4];\n_Static_assert(sizeof b == 16, \"b\");\n",
    );
}

/// A grouped declarator's initializer is an initializer like any other: it
/// sizes an incomplete array and is checked for excess elements.
#[test]
fn diagnostics_grouped_declarator_initializer_is_checked() {
    compile_expect_ok(
        "grouped_init_sizes",
        "int (a)[] = {1, 2, 3};\n_Static_assert(sizeof a == 12, \"a\");\n",
    );
    compile_expect_warning(
        "grouped_init_excess",
        "int (a)[2] = {1, 2, 3};\n",
        "excess elements in array initializer",
    );
}

/// `typedef int A, B __attribute__((aligned(16)));` aligns `B` alone. The
/// later-declarator binder never folded the alignment into the typedef.
#[test]
fn diagnostics_trailing_alignment_reaches_every_typedef_declarator() {
    compile_expect_ok(
        "typedef_align_later",
        "typedef int A, B __attribute__((aligned(16)));\n\
         _Static_assert(_Alignof(B) == 16, \"B\");\n\
         _Static_assert(_Alignof(A) == 4, \"A\");\n",
    );
    compile_expect_ok(
        "typedef_align_grouped",
        "typedef int (C) __attribute__((aligned(16)));\n\
         _Static_assert(_Alignof(C) == 16, \"C\");\n\
         int (o) __attribute__((aligned(32)));\n\
         _Static_assert(__alignof__(o) == 32, \"o\");\n",
    );
}

/// C11 6.7.5p2 forbids `_Alignas` on a function, and a grouped function
/// declarator is still a function -- bound as one, so it is no lvalue either.
#[test]
fn diagnostics_grouped_function_declarator_declares_a_function() {
    compile_expect_error(
        "grouped_fn_alignas",
        "_Alignas(16) void (f)(void);\n",
        "_Alignas cannot be applied to a function",
    );
    compile_expect_error(
        "grouped_fn_not_lvalue",
        "void (f)(void);\nvoid g(void){ f = 0; }\n",
        "lvalue required as left operand of assignment",
    );
}

/// C17 6.7.6.2p1: `static` and type qualifiers in an array declarator, and
/// `[*]`, belong to a function parameter. `parse_declarator` took them
/// anywhere. A K&R parameter declaration is a parameter too, but not in
/// prototype scope, so `static` is fine there and `[*]` is not.
#[test]
fn diagnostics_parameter_only_array_declarators_are_rejected_elsewhere() {
    for (name, src) in [
        ("arr_static_file", "int a[static 3];\n"),
        ("arr_const_file", "int a[const 3];\n"),
        (
            "arr_static_block",
            "void f(void){ int a[static 3]; (void)a; }\n",
        ),
        ("arr_static_typename", "int x = sizeof(int[static 2]);\n"),
    ] {
        compile_expect_error(
            name,
            src,
            "static or type qualifiers in non-parameter array declarator",
        );
    }
    for (name, src) in [
        ("arr_star_file", "int a[*];\n"),
        ("arr_star_block", "void f(void){ int a[*]; (void)a; }\n"),
        ("arr_star_nested", "int (*p)[*];\n"),
        ("arr_star_member", "struct S { int n; int a[*]; };\n"),
        ("arr_star_typename", "int x = sizeof(int[*]);\n"),
        ("arr_star_knr", "int f(a) int a[*]; { return a[0]; }\n"),
    ] {
        compile_expect_error(
            name,
            src,
            "'[*]' not allowed in other than function prototype scope",
        );
    }
    compile_expect_ok(
        "arr_parameter_forms",
        "void f(int n, int a[*]);\nvoid g(int a[static 3]);\nvoid h(int a[const 3]);\n\
         void i(int (*a)[*]);\nvoid (*fp)(int a[static 3]);\n\
         int k(a) int a[static 3]; { return a[0]; }\n",
    );
}

/// C11 6.7.1p2: `_Thread_local` shall not appear with `auto` or `register`,
/// at file scope as much as in a block.
#[test]
fn diagnostics_thread_local_with_auto_or_register_at_file_scope() {
    compile_expect_error(
        "tl_register_file",
        "_Thread_local register int x;\n",
        "_Thread_local cannot be combined with register",
    );
    compile_expect_error(
        "tl_auto_file",
        "_Thread_local auto int x;\n",
        "_Thread_local cannot be combined with auto",
    );
}

/// A variably modified type at file scope is refused however it is spelled:
/// through a grouped declarator whose inner declarator holds the extent, or
/// through `typeof`.
#[test]
fn diagnostics_variably_modified_file_scope_through_grouping_or_typeof() {
    for (name, src) in [
        ("fs_vla_inner_grouped", "int n;\nint (*p[n]);\n"),
        ("fs_vla_typeof", "int n;\ntypeof(int[n]) x;\n"),
    ] {
        compile_expect_error(name, src, "file scope");
    }
}

/// A function declarator takes no initializer, whichever position it holds.
#[test]
fn diagnostics_function_declarator_with_initializer_is_rejected() {
    for (name, src) in [
        ("fn_init_first", "int f(void) = 0;\n"),
        ("fn_init_later", "int x, f(void) = 0;\n"),
        ("fn_init_block", "void g(void){ int f(void) = 0; }\n"),
    ] {
        compile_expect_error(name, src, "function 'f' is initialized like a variable");
    }
}

/// A conflicting redeclaration is reported where the declarator is, as gcc
/// does, not where its declaration began -- the three binders disagreed.
#[test]
fn diagnostics_redeclaration_is_reported_at_the_declarator() {
    compile_expect_error(
        "redecl_at_declarator",
        "int x;\ndouble\n  y, x;\n",
        ":3:6: error: conflicting types for 'x'",
    );
    compile_expect_error(
        "redecl_first_at_declarator",
        "int x;\ndouble\n  x;\n",
        ":3:3: error: conflicting types for 'x'",
    );
    compile_expect_error(
        "typedef_redef_at_declarator",
        "typedef int T;\ntypedef\n  double U, T;\n",
        ":3:13: error: typedef 'T' redefined",
    );
}

/// C17 6.5.8p2: `<`, `>`, `<=` and `>=` take real or pointer operands, and a
/// complex value has no ordering -- constant or not, either side. gcc: "invalid
/// operands to binary <".
#[test]
fn diagnostics_relational_operator_rejects_a_complex_operand() {
    for (name, src) in [
        ("const", "int k = (1.0 + 2.0i) < (1.0 + 2.0i);\n"),
        (
            "runtime",
            "int g(void) { _Complex double a = 1, b = 2; return a >= b; }\n",
        ),
        ("right", "int g(_Complex int z) { return 1.0 > z; }\n"),
    ] {
        compile_expect_error(
            &format!("complex_relational_{name}"),
            src,
            "invalid operands to binary",
        );
    }
}

/// `==` and `!=` do take a complex operand (6.5.9p2), and fold as constants.
#[test]
fn diagnostics_equality_accepts_a_complex_operand() {
    compile_expect_ok(
        "complex_equality",
        "int f = (_Complex float)(0.5) == 0.5;\n\
         int g(_Complex double a, double b) { return (a == b) + (a != 2.0i); }\n",
    );
}

// ==== numeric escapes out of range (C17 6.4.4.4p9) ====

/// An octal or hex escape's value must be representable in the literal's
/// element type: `unsigned char` for a plain literal, and the unsigned type
/// of `wchar_t`, `char16_t` or `char32_t` for a prefixed one. gcc warns and
/// truncates; c17 was silent. A constraint gcc only warns about is an error
/// here, and `-fpermissive` makes it a warning with gcc's truncation.
#[test]
fn diagnostics_escape_out_of_range() {
    for (name, src, msg) in [
        (
            "esc_oct_char",
            "int c = '\\400';\n",
            "octal escape sequence out of range",
        ),
        (
            "esc_oct_str",
            "char s[] = \"a\\777\";\n",
            "octal escape sequence out of range",
        ),
        (
            "esc_hex_char",
            "int c = '\\x100';\n",
            "hex escape sequence out of range",
        ),
        (
            "esc_hex_str",
            "char s[] = \"\\x123\";\n",
            "hex escape sequence out of range",
        ),
        (
            "esc_hex_u16",
            "unsigned short s[] = u\"\\x12345\";\n",
            "hex escape sequence out of range",
        ),
        (
            "esc_hex_u16c",
            "int c = u'\\x10000';\n",
            "hex escape sequence out of range",
        ),
        (
            "esc_hex_u32",
            "unsigned s[] = U\"\\x100000000\";\n",
            "hex escape sequence out of range",
        ),
        (
            "esc_hex_wide",
            "int c = L'\\x123456789';\n",
            "hex escape sequence out of range",
        ),
        // A narrow piece concatenated to a prefixed one takes its type
        // (6.4.5p5), so the bound is the prefixed one's...
        (
            "esc_hex_concat",
            "unsigned short s[] = \"\\x12345\" u\"a\";\n",
            "hex escape sequence out of range",
        ),
        (
            "esc_if",
            "#if '\\x100'\n#endif\nint x;\n",
            "hex escape sequence out of range",
        ),
    ] {
        let strict = compile_with(name, src, &[]);
        assert!(
            !strict.success && strict.stderr.contains("error:") && strict.stderr.contains(msg),
            "{name}: expected an error mentioning {msg:?}:\n{}",
            strict.stderr
        );
        let lax = compile_with(name, src, &["-fpermissive"]);
        assert!(
            lax.success && lax.stderr.contains("warning:") && lax.stderr.contains(msg),
            "{name}: -fpermissive should warn {msg:?}:\n{}",
            lax.stderr
        );
    }
}

/// The bound is the element type's, and a value's leading zeros do not count.
#[test]
fn diagnostics_escape_in_range_is_accepted() {
    compile_expect_no_diagnostic(
        "esc_in_range",
        "char a[] = \"\\377\\xff\\x00000041\\0\";\n\
         int b = '\\377' + '\\xff';\n\
         unsigned short c[] = u\"\\xffff\\777\";\n\
         unsigned d[] = U\"\\xffffffff\\x0000000000041\";\n\
         int e = L'\\xffffffff' + L'\\777';\n\
         unsigned short f[] = \"\\xff\" u\"a\";\n\
         #if '\\xff' && u'\\xffff' && U'\\xffffffff'\n\
         int g;\n\
         #endif\n",
        "escape sequence out of range",
    );
}

/// Under `-fpermissive` the program keeps gcc's truncation to the low bits.
#[test]
fn diagnostics_escape_out_of_range_truncates_under_fpermissive() {
    let src = r#"
typedef __CHAR16_TYPE__ char16_t;
int main(void) {
    const char16_t *u = u"\x12345";
    if ((unsigned char)"\x141"[0] != 0x41) return 1;
    if ((unsigned char)'\777' != 0xff) return 2;
    if (u[0] != 0x2345) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("esc_truncate", src, &["-fpermissive".to_string()]),
        0
    );
}

/// An out-of-range escape is reported where the literal is, not where the
/// next token is.
///
/// The character-constant paths bind `token_pos` from the token they consume
/// and then passed `self.current_pos()`, which after the `consume` is the
/// *following* token. With the terminator on a line of its own the
/// diagnostic named the wrong line outright:
///
/// ```text
///     2 |   char c = '\400'
///     3 |   ;
///     c17    3:3: error: octal escape sequence out of range
///     clang  2:12: error: octal escape sequence out of range
/// ```
///
/// The string-literal path already reported from each piece's own position,
/// and is the control here.
#[test]
fn diagnostics_escape_out_of_range_names_the_literals_line() {
    for (name, src) in [
        // The escape is on line 2; the `;` that follows is on line 3.
        (
            "esc_pos_char",
            "int main(void) {\n  char c = '\\400'\n  ;\n}\n",
        ),
        (
            "esc_pos_wchar",
            "int main(void) {\n  int c = u'\\x10000'\n  ;\n}\n",
        ),
        (
            "esc_pos_str",
            "int main(void) {\n  char s[] = \"a\\777\"\n  ;\n}\n",
        ),
    ] {
        let out = compile_with(name, src, &[]);
        let line = out
            .stderr
            .lines()
            .find(|l| l.contains("escape sequence out of range"))
            .unwrap_or_else(|| panic!("{name}: no escape diagnostic in:\n{}", out.stderr));
        let at = line
            .rsplit_once(".c:")
            .map(|(_, rest)| rest)
            .unwrap_or(line);
        assert!(
            at.starts_with("2:"),
            "{name}: the escape is on line 2, but the diagnostic says {at:?}"
        );
    }
}

/// A member of a `const` object is itself `const`, so writing it is a
/// constraint violation.
///
/// C17 6.5.2.3p3/p4 gives `s.m` the *so-qualified* version of the member's
/// type, and 6.5.16p2 requires a modifiable lvalue on the left of an
/// assignment. c17 took the member's declared type unqualified, so
/// `check_const_assignment` saw an ordinary `int` and every one of these was
/// accepted silently. gcc and clang reject them.
#[test]
fn diagnostics_writing_a_member_of_a_const_object_is_rejected() {
    compile_expect_error(
        "const_aggregate_member",
        "struct S { int a; };\nconst struct S cs = {1};\nvoid f(void){ cs.a = 2; }\n",
        "read-only",
    );
    compile_expect_error(
        "const_aggregate_member_arrow",
        "struct S { int a; };\nvoid f(const struct S *p){ p->a = 2; }\n",
        "read-only",
    );
    compile_expect_error(
        "const_aggregate_member_nested",
        "struct T { int x; };\nstruct S { struct T t; };\n\
         const struct S cs;\nvoid f(void){ cs.t.x = 2; }\n",
        "read-only",
    );
    compile_expect_error(
        "const_aggregate_member_increment",
        "struct S { int a; };\nconst struct S cs;\nvoid f(void){ cs.a++; }\n",
        "read-only",
    );
    compile_expect_error(
        "const_aggregate_element",
        "struct S { int a; };\nconst struct S cs[2];\nvoid f(void){ cs[1].a = 2; }\n",
        "read-only",
    );
}

/// The other direction: the same shapes without the `const` must still compile,
/// so the checks above cannot pass by rejecting every member assignment.
#[test]
fn diagnostics_writing_a_member_of_a_plain_object_is_accepted() {
    compile_expect_ok(
        "plain_aggregate_member",
        "struct S { int a; };\nstruct S s;\nvoid f(void){ s.a = 2; }\n\
         void g(struct S *p){ p->a = 2; }\n\
         struct T { struct S in; };\nstruct T t;\nvoid h(void){ t.in.a = 2; }\n\
         struct S arr[2];\nvoid i(void){ arr[1].a = 2; }\n\
         void j(void){ s.a++; }\n",
    );
    // A `const` *pointer* to a non-const object leaves the pointee writable:
    // the qualifier is on the pointer, not on what it points at.
    compile_expect_ok(
        "const_pointer_not_pointee",
        "struct S { int a; };\nvoid f(struct S *const p){ p->a = 2; }\n",
    );
}

/// An *array* member of a `const` object is an array of `const`, so writing an
/// element of it is a constraint violation too.
///
/// C17 6.7.3p10: where an array type is qualified, the element type is
/// so-qualified and the array is not -- which is what a subscript reads. So the
/// so-qualified version of `int [4]` is an array of `const int`, and
/// `cs.arr[0] = 1` is a write to a `const int`. Qualifying the array itself
/// instead would leave the element an ordinary `int` and accept the write.
#[test]
fn diagnostics_writing_an_array_member_of_a_const_object_is_rejected() {
    compile_expect_error(
        "const_aggregate_array_member",
        "struct S { int arr[4]; };\nconst struct S cs;\nvoid f(void){ cs.arr[0] = 2; }\n",
        "read-only",
    );
    compile_expect_error(
        "const_aggregate_array_member_2d",
        "struct S { int grid[2][2]; };\nvoid f(const struct S *p){ p->grid[1][1] = 2; }\n",
        "read-only",
    );
    // The control: the same writes through an unqualified object.
    compile_expect_ok(
        "plain_aggregate_array_member",
        "struct S { int arr[4]; int grid[2][2]; };\nstruct S s;\n\
         void f(void){ s.arr[0] = 2; s.grid[1][1] = 3; }\n\
         void g(struct S *p){ p->arr[0] = 2; }\n",
    );
}

/// A `case` label whose conversion to the controlling type changes its value
/// is diagnosed, and two labels that become equal are a constraint violation.
///
/// C17 6.8.4.2p5 converts each label to the promoted type of the controlling
/// expression, and p3 forbids two labels in one switch having the same value
/// *after* that conversion. c17 kept labels at full width, so it diagnosed
/// neither: `case 4294967296LL` in an `int` switch silently became `case 0`,
/// and sitting beside a real `case 0` it was silently accepted.
///
/// gcc and clang both warn on the value-changing conversion and reject the
/// collision.
#[test]
fn diagnostics_a_case_label_outside_the_controlling_type_is_diagnosed() {
    compile_expect_warning(
        "case_label_overflow",
        "int f(int x){ switch(x){ case 4294967296LL: return 1; default: return 2; } }\n",
        "case",
    );
    compile_expect_error(
        "case_label_duplicate_after_conversion",
        "int f(int x){ switch(x){ case 0: return 1; case 4294967296LL: return 2; } return 0; }\n",
        "duplicate",
    );
}

/// The other direction: a conversion that preserves the value is silent, so
/// the check above cannot pass by warning about every label.
///
/// `case -1` in a `switch` on `unsigned` converts to 4294967295 and genuinely
/// matches it -- the conversion is value-changing in representation but well
/// defined and intended, which is why gcc and clang say nothing here either.
#[test]
fn diagnostics_a_case_label_inside_the_controlling_type_is_silent() {
    compile_expect_no_diagnostic(
        "case_label_in_range",
        "int f(int x){ switch(x){ case -1: return 1; case 7: return 3; default: return 2; } }\n",
        "case",
    );
    compile_expect_no_diagnostic(
        "case_label_negative_in_unsigned",
        "int f(unsigned x){ switch(x){ case -1: return 1; default: return 2; } }\n",
        "case",
    );
    compile_expect_ok(
        "case_labels_distinct_after_conversion",
        "int f(int x){ switch(x){ case 0: return 1; case 1: return 2; default: return 3; } }\n",
    );
}
