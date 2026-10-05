use crate::test_compile::{
    compile, compile_expect_error, compile_expect_ok, compile_expect_warning,
};

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
    let run = compile("fnptr_no_over_fire", src, &[]);
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

    for silencer in ["-w", "-Wno-function-pointer-conv"] {
        let run = compile("fnptr_silence", src, &[silencer]);
        assert!(run.success, "{silencer} should be accepted: {}", run.stderr);
        assert!(
            !run.stderr.contains("ISO C forbids"),
            "{silencer} should silence the conversion warning, got:\n{}",
            run.stderr
        );
    }

    // An unrelated -Wno- must not silence it, or the flag name means nothing.
    let run = compile("fnptr_silence", src, &["-Wno-unused"]);
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
    for silencer in ["-w", "-Wno-attributes"] {
        let run = compile("transparent_union_silence", src, &[silencer]);
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
