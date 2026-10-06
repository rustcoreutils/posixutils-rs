use crate::test_compile::{
    compile_accepted, compile_expect_error, compile_expect_ok, compile_rejected_with,
};

// ============================================================================
// A declaration that declares nothing (C17 6.7p2)
// ============================================================================

/// 6.7p2 wants a declarator, a tag, or enumeration members. These have none.
///
/// All of them were **accepted silently** at first. gcc refuses a function
/// specifier here, and `auto` or `register` at file scope; the rest it warns
/// about, with a pedwarn that `-pedantic-errors` makes an error. c17 gives
/// each one gcc's verdict and gcc's words.
#[test]
fn diagnostics_declaration_that_declares_nothing_is_rejected() {
    for (name, src, msg) in [
        (
            "declnothing_inline",
            "int inline;\n",
            "'inline' in empty declaration",
        ),
        (
            "declnothing_bare_inline",
            "inline;\n",
            "'inline' in empty declaration",
        ),
        (
            "declnothing_noreturn",
            "_Noreturn;\n",
            "'_Noreturn' in empty declaration",
        ),
        (
            "declnothing_register",
            "int register;\n",
            "'register' in file-scope empty declaration",
        ),
        (
            "declnothing_auto",
            "auto;\n",
            "'auto' in file-scope empty declaration",
        ),
        (
            "declnothing_block_inline",
            "void f(void){ inline; }\n",
            "'inline' in empty declaration",
        ),
    ] {
        compile_expect_error(name, src, msg);
    }
}

/// The empty declarations gcc only warns about: a type named for nothing, or
/// a storage class, `_Thread_local` or qualifier that applies to nothing.
/// Warnings by default, errors under `-pedantic-errors`.
#[test]
fn diagnostics_declaration_that_declares_nothing_is_gccs_pedwarn() {
    const USELESS_TYPE: &str = "useless type name in empty declaration";
    const EMPTY: &str = "empty declaration";
    for (name, src, first) in [
        ("declnothing_int", "int;\n", USELESS_TYPE),
        ("declnothing_unsigned", "unsigned;\n", USELESS_TYPE),
        ("declnothing_typedef_int", "int typedef;\n", USELESS_TYPE),
        (
            "declnothing_block_register",
            "void f(void){ int register; }\n",
            USELESS_TYPE,
        ),
        (
            "declnothing_static",
            "static;\n",
            "useless storage class specifier in empty declaration",
        ),
        (
            "declnothing_extern",
            "extern;\n",
            "useless storage class specifier in empty declaration",
        ),
        (
            "declnothing_block_static",
            "void f(void){ static; }\n",
            "useless storage class specifier in empty declaration",
        ),
        (
            "declnothing_block_auto",
            "void f(void){ auto; }\n",
            "useless storage class specifier in empty declaration",
        ),
        (
            "declnothing_thread_local",
            "_Thread_local;\n",
            "useless '_Thread_local' in empty declaration",
        ),
        (
            "declnothing_const",
            "const;\n",
            "useless type qualifier in empty declaration",
        ),
        (
            "declnothing_volatile",
            "volatile;\n",
            "useless type qualifier in empty declaration",
        ),
        (
            "declnothing_untagged_struct",
            "struct { int a; };\n",
            "unnamed struct/union that defines no instances",
        ),
    ] {
        let warned = compile_accepted(name, src, &[]);
        assert!(
            warned.contains("warning:") && warned.contains(first),
            "{name}: expected a warning {first:?}:\n{warned}"
        );
        // With no type named, gcc's pedwarn is a separate "empty
        // declaration" after the warning naming the useless specifier.
        let names_a_specifier = first != USELESS_TYPE && !name.contains("struct");
        assert_eq!(
            warned
                .lines()
                .any(|l| l.ends_with(&format!("warning: {EMPTY}"))),
            names_a_specifier,
            "{name}:\n{warned}"
        );
        compile_rejected_with(name, src, &["-pedantic-errors"]);
    }
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

/// A tag declaration with a specifier that applies to nothing: the tag is
/// declared, so there is no "empty declaration" pedwarn, but the specifier
/// draws gcc's plain warning naming it -- the storage class first, then
/// `_Thread_local`, then a qualifier, then `_Alignas`, and only the first.
/// These were silent: a struct, union or enum specifier left the check
/// before it looked at any specifier.
#[test]
fn diagnostics_useless_specifier_on_a_tag_declaration_warns() {
    const STORAGE: &str = "useless storage class specifier in empty declaration";
    const QUALIFIER: &str = "useless type qualifier in empty declaration";
    for (name, src, want) in [
        ("tagspec_static", "static struct S { int a; };\n", STORAGE),
        ("tagspec_extern", "extern struct S { int a; };\n", STORAGE),
        ("tagspec_typedef", "typedef struct S { int a; };\n", STORAGE),
        ("tagspec_union", "static union U { int a; };\n", STORAGE),
        ("tagspec_enum", "static enum E { A };\n", STORAGE),
        ("tagspec_anon_enum", "static enum { A };\n", STORAGE),
        ("tagspec_first_ref", "static struct New;\n", STORAGE),
        (
            "tagspec_static_const",
            "static const struct S { int a; };\n",
            STORAGE,
        ),
        ("tagspec_const", "const struct S { int a; };\n", QUALIFIER),
        (
            "tagspec_volatile",
            "volatile struct S { int a; };\n",
            QUALIFIER,
        ),
        (
            "tagspec_atomic",
            "_Atomic struct S { int a; };\n",
            QUALIFIER,
        ),
        ("tagspec_const_enum", "const enum E { A };\n", QUALIFIER),
        ("tagspec_const_first_ref", "const struct New;\n", QUALIFIER),
        (
            "tagspec_thread_local",
            "_Thread_local struct S { int a; };\n",
            "useless '_Thread_local' in empty declaration",
        ),
        (
            "tagspec_gnu_thread",
            "__thread struct S { int a; };\n",
            "useless '__thread' in empty declaration",
        ),
        (
            "tagspec_thread_local_ref",
            "struct S { int a; };\n_Thread_local struct S;\n",
            "useless '_Thread_local' in empty declaration",
        ),
        (
            "tagspec_alignas",
            "_Alignas(8) struct S { int a; };\n",
            "useless '_Alignas' in empty declaration",
        ),
        (
            "tagspec_alignas_union",
            "_Alignas(16) union U { int a; };\n",
            "useless '_Alignas' in empty declaration",
        ),
        (
            "tagspec_block_static",
            "void f(void) { static struct L { int a; }; }\n",
            STORAGE,
        ),
        (
            "tagspec_block_register",
            "void f(void) { register struct L { int a; }; }\n",
            STORAGE,
        ),
        (
            "tagspec_block_auto",
            "void f(void) { auto struct L { int a; }; }\n",
            STORAGE,
        ),
        (
            "tagspec_block_const",
            "void f(void) { const struct L { int b; }; }\n",
            QUALIFIER,
        ),
    ] {
        // A plain warning, which `-pedantic-errors` leaves a warning.
        let warned = compile_accepted(name, src, &["-pedantic-errors"]);
        let diags: Vec<&str> = warned
            .lines()
            .filter(|l| l.contains(": warning: "))
            .collect();
        assert_eq!(diags.len(), 1, "{name}: expected one warning:\n{warned}");
        assert!(
            diags[0].ends_with(&format!("warning: {want}")),
            "{name}:\n{warned}"
        );
    }
}

/// A reference to a tag that is already declared, with a storage class,
/// qualifier or `_Alignas`, declares nothing new: gcc's pedwarn says the tag
/// is not redeclared, and `-pedantic-errors` makes it an error.
#[test]
fn diagnostics_tag_reference_with_specifier_does_not_redeclare() {
    for (name, src, what) in [
        (
            "tagref_static",
            "struct S { int a; };\nstatic struct S;\n",
            "storage class specifier",
        ),
        (
            "tagref_static_alignas",
            "struct S { int a; };\nstatic _Alignas(4) struct S;\n",
            "storage class specifier",
        ),
        (
            "tagref_enum",
            "enum E { A };\nstatic enum E;\n",
            "storage class specifier",
        ),
        (
            "tagref_const",
            "struct S { int a; };\nconst struct S;\n",
            "type qualifier",
        ),
        (
            "tagref_block",
            "struct S { int a; };\nvoid f(void) { const struct S; }\n",
            "type qualifier",
        ),
        (
            "tagref_alignas",
            "struct S { int a; };\n_Alignas(8) struct S;\n",
            "'_Alignas'",
        ),
        (
            "tagref_alignas_enum",
            "enum E { A };\n_Alignas(4) enum E;\n",
            "'_Alignas'",
        ),
    ] {
        let want = format!("empty declaration with {what} does not redeclare tag");
        let warned = compile_accepted(name, src, &[]);
        let diags: Vec<&str> = warned
            .lines()
            .filter(|l| l.contains(": warning: "))
            .collect();
        assert_eq!(diags.len(), 1, "{name}: expected one warning:\n{warned}");
        assert!(
            diags[0].ends_with(&format!("warning: {want}")),
            "{name}:\n{warned}"
        );
        let refused = compile_rejected_with(name, src, &["-pedantic-errors"]);
        assert!(
            refused.contains(&format!("error: {want}")),
            "{name}:\n{refused}"
        );
    }
}

/// The specifiers gcc refuses on a tag declaration, as on any other empty
/// one.
#[test]
fn diagnostics_refused_specifier_on_a_tag_declaration() {
    for (name, src, msg) in [
        (
            "tagbad_register",
            "register struct S { int a; };\n",
            "'register' in file-scope empty declaration",
        ),
        (
            "tagbad_auto",
            "auto struct S { int a; };\n",
            "'auto' in file-scope empty declaration",
        ),
        (
            "tagbad_inline",
            "inline struct S { int a; };\n",
            "'inline' in empty declaration",
        ),
        (
            "tagbad_noreturn",
            "_Noreturn struct S { int a; };\n",
            "'_Noreturn' in empty declaration",
        ),
        (
            "tagbad_restrict",
            "restrict struct S { int a; };\n",
            "invalid use of 'restrict'",
        ),
    ] {
        let refused = compile_rejected_with(name, src, &[]);
        assert!(
            refused.contains(&format!("error: {msg}")),
            "{name}:\n{refused}"
        );
        assert!(!refused.contains("useless"), "{name}:\n{refused}");
    }
    // A reference draws both the pedwarn and the refusal.
    let refused = compile_rejected_with(
        "tagbad_register_ref",
        "struct S { int a; };\nregister struct S;\n",
        &[],
    );
    assert!(
        refused.contains(
            "warning: empty declaration with storage class specifier does not redeclare tag"
        ) && refused.contains("error: 'register' in file-scope empty declaration"),
        "{refused}"
    );
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
