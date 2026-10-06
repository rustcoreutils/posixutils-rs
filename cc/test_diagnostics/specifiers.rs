use crate::test_compile::{
    compile, compile_expect_error, compile_expect_no_diagnostic, compile_expect_ok,
    compile_expect_warning,
};

// ============================================================================
// One declaration-specifier loop (C17 6.7.2, 6.7.7)
// ============================================================================

/// Compile `src` and return its stderr, requiring that it was rejected.
fn rejected_stderr(name: &str, src: &str, extra: &[&str]) -> String {
    let run = compile(name, src, extra);
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
        &["--target=aarch64-apple-darwin"],
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
/// truncates, an error only under `-pedantic-errors`; c17 was silent, and now
/// does as gcc does.
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
        let warned = compile(name, src, &[]);
        assert!(
            warned.success && warned.stderr.contains(&format!("warning: {msg}")),
            "{name}: expected a warning {msg:?}:\n{}",
            warned.stderr
        );
        let strict = compile(name, src, &["-pedantic-errors"]);
        assert!(
            !strict.success && strict.stderr.contains(&format!("error: {msg}")),
            "{name}: -pedantic-errors should refuse it with {msg:?}:\n{}",
            strict.stderr
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
        let out = compile(name, src, &[]);
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
