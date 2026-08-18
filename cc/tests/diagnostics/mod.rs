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

use crate::common::{compile_expect_error, compile_expect_ok, compile_expect_warning};

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
        "typedef of a variable-length array type",
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

/// #L1's implicit-int check is driven by a parser-wide `saw_explicit_type`
/// flag. The struct/union/enum/typeof arms of `parse_type_specifier` return
/// early without setting it, so a specifier-less call — a K&R identifier list,
/// whose undeclared parameters have type `int` by C17 6.9.1p6 — left the flag
/// false and the *next* declaration inherited the complaint.
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
        "incompatible type",
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
        "call expects 0 arguments",
    );
    compile_expect_error(
        "void_definition_too_many_args",
        "int f(void){ return 0; }\nint main(void){ return f(1, 2); }\n",
        "call expects 0 arguments",
    );
    compile_expect_error(
        "void_proto_one_arg",
        "int f(void);\nint main(void){ return f(7); }\nint f(void){ return 0; }\n",
        "call expects 0 arguments",
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
        // rejected with "call expects 2 arguments, but 1 given".
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
            "incompatible types returning",
        ),
        (
            "return_struct_mismatch",
            "struct A{int x;}; struct B{int x;};\nstruct A f(void){ struct B b; return b; }\n",
            "incompatible types returning",
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

/// glibc declares the socket calls with a union parameter carrying
/// `__attribute__((transparent_union))`, so a caller may hand them any one of
/// its member types. c17 does not record that attribute, and the first cut of
/// the argument check rejected `sendto(..., SAS2SA(&addr), ...)` -- two lines
/// of CPython's socketmodule.c, and with them every socket program on the
/// platform.
///
/// An argument matching some member of a union parameter is therefore
/// accepted. This pins both the real header call and the synthetic shape
/// behind it, so that implementing the attribute properly has something to
/// tighten against.
#[test]
fn diagnostics_union_parameter_accepts_a_member_type() {
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
    compile_expect_ok(
        "union_parameter_member_type",
        "union U { int *ip; char *cp; };
int g(union U);
void f(void){ int *p = 0; (void)g(p); }
",
    );
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
/// that changes what the type *is*: `__attribute__((vector_size(16)))` left
/// the type scalar and every operation on it scalar, so the program computed
/// on one element instead of all of them, with nothing said.
///
/// So: an error for `vector_size`, which cannot be ignored correctly and which
/// no C system header uses; a warning for everything else unrecognised.
#[test]
fn diagnostics_unimplemented_attributes_are_reported() {
    compile_expect_error(
        "vector_size_unimplemented",
        "typedef int V __attribute__((vector_size(16)));\nint main(void){ return 0; }\n",
        "'vector_size' attribute is not implemented",
    );
    compile_expect_warning(
        "unknown_attribute_warns",
        "typedef int T __attribute__((totally_made_up));\nint main(void){ return 0; }\n",
        "attribute directive ignored",
    );
    // `mode` changes the type too, but glibc declares `register_t` with it in
    // <sys/types.h>, so refusing would reject every program that includes it.
    compile_expect_warning(
        "mode_warns_rather_than_refusing",
        "typedef int W __attribute__((__mode__(__word__)));\nint main(void){ return 0; }\n",
        "'__mode__' attribute is not implemented",
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
#if !__has_attribute(ms_abi) || !__has_attribute(gnu_inline)
#error "__has_attribute must admit the attributes the compiler honours"
#endif
#if __has_attribute(vector_size)
#error "__has_attribute must not claim an attribute the compiler ignores"
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
        ("wide_into_unsigned_array", "unsigned a[] = L\"ab\";\n"),
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
        ("wide_into_int", "int a[] = L\"ab\";\n"),
        ("wide_into_const_int", "const int a[] = L\"ab\";\n"),
        ("u16_into_ushort", "unsigned short a[] = u\"ab\";\n"),
        ("u32_into_uint", "unsigned int a[] = U\"ab\";\n"),
        ("array_from_braces", "int a[] = {1,2,3};\n"),
        ("pointer_from_string", "const char *p = \"hi\";\n"),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A bit-field wider than 64 bits has no carrier here: the value mask is a
/// `u64` and `bitfield_storage_type` has no arm for a sixteen-byte unit.
///
/// `unsigned __int128 a:100` read back a wrong value in a release build and
/// **panicked the compiler** in a debug one. gcc supports it, so refusing is a
/// divergence -- but a diagnostic naming the limit beats a panic, and
/// `__int128` is a GNU extension. Widths up to 64 of such a type still work
/// and still agree with gcc, so the cap is on the width, not the type.
#[test]
fn diagnostics_bitfield_wider_than_its_carrier_is_rejected() {
    for (name, src) in [
        ("bf_i128_65", "struct S { unsigned __int128 a:65; };\n"),
        ("bf_i128_100", "struct S { unsigned __int128 a:100; };\n"),
        ("bf_i128_128", "struct S { unsigned __int128 a:128; };\n"),
        ("bf_i128_signed", "struct S { __int128 a:96; };\n"),
        ("bf_i128_unnamed", "struct S { unsigned __int128 : 96; };\n"),
    ] {
        compile_expect_error(name, src, "c17 carries at most 64 bits");
    }
}

/// ...and everything at or below the carrier's width still compiles, including
/// a 64-bit field of a 128-bit type.
#[test]
fn diagnostics_bitfields_within_the_carrier_are_accepted() {
    for (name, src) in [
        ("bf_i128_64", "struct S { unsigned __int128 a:64; };\n"),
        ("bf_i128_32", "struct S { unsigned __int128 a:32; };\n"),
        ("bf_i128_1", "struct S { unsigned __int128 a:1; };\n"),
        ("bf_ull_64", "struct S { unsigned long long a:64; };\n"),
        ("bf_int_32", "struct S { int a:32; };\n"),
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
