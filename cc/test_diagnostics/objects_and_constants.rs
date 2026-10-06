use crate::test_compile::{compile_expect_error, compile_expect_ok, compile_expect_warning};

// ============================================================================
// Array compatibility when one side has no extent (C17 6.7.6.2p6)
// ============================================================================

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
/// The array type's extent settles it: `int[]` is `ArrayExtent::Unknown` and
/// incomplete, while `int[n]` is `ArrayExtent::Variable` and complete, so a
/// VLA's `sizeof` keeps working while an incomplete array's is refused.
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

/// A floating constant an implicit conversion takes out of an integer
/// type's range draws gcc's `-Woverflow` warning in code as it does in a
/// static initializer: `return`, initialization, assignment and a prototyped
/// argument. An explicit cast is silent, as in gcc, and `-Wno-overflow`
/// silences the rest.
#[test]
fn diagnostics_saturating_implicit_conversion_warns() {
    for (name, src, to) in [
        ("sat_return", "int f(void) { return 1e10; }\n", "int"),
        (
            "sat_init",
            "int f(void) { int x = -1e10; return x; }\n",
            "int",
        ),
        (
            "sat_assign",
            "unsigned f(void) { unsigned u; u = -5.0; return u; }\n",
            "unsigned int",
        ),
        (
            "sat_arg",
            "int g(int);\nint f(void) { return g(1e10); }\n",
            "int",
        ),
    ] {
        let want = format!("overflow in conversion from 'double' to '{to}' changes value");
        compile_expect_warning(name, src, &want);
        let quiet = crate::test_compile::compile_expect_warning_with(
            name,
            src,
            &["-Wno-overflow".to_string()],
        );
        assert!(!quiet.contains("overflow"), "{name}: {quiet}");
    }
    crate::test_compile::compile_expect_no_diagnostic(
        "sat_cast",
        "int f(void) { return (int)1e10 + (unsigned char)-1.0; }\n",
        "overflow",
    );
}
