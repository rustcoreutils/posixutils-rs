use crate::test_compile::{compile_expect_error, compile_expect_ok};

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
