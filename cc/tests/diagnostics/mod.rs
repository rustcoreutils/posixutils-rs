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
