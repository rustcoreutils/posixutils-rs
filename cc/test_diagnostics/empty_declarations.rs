use crate::test_compile::{compile_expect_error, compile_expect_ok};

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
