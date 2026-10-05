use crate::test_compile::{compile_expect_error, compile_expect_ok, compile_expect_warning};

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
