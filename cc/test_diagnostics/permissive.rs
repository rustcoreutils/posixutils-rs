use crate::test_compile::{
    compile, compile_expect_error, compile_expect_ok, compile_expect_warning_with,
};

// ============================================================================
// -fpermissive — two pre-C99 constructs, error by default
// ============================================================================

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

    let strict = compile("permissive_off_int", src, &[]);
    assert!(!strict.success, "implicit int must be an error by default");
    assert!(
        strict.stderr.contains("error:") && strict.stderr.contains("type specifier missing"),
        "default build lost the implicit-int error:\n{}",
        strict.stderr
    );

    let lax = compile("permissive_on_int", src, &["-fpermissive"]);
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

    let strict = compile("permissive_off_fn", src, &[]);
    assert!(
        !strict.success && strict.stderr.contains("undeclared identifier"),
        "a call to an undeclared function must be an error by default:\n{}",
        strict.stderr
    );

    let lax = compile("permissive_on_fn", src, &["-fpermissive"]);
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
        let run = compile(name, src, &["-fpermissive"]);
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
        let run = compile(name, src, &["-fpermissive"]);
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
        let warned = compile_expect_warning_with(name, src, &["-fpermissive".to_string()]);
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

/// A `vector_size` value passed to or returned from a function is refused
/// only where the target's convention has no type that travels as gcc
/// passes it: a one-lane `float` vector on aarch64. On x86-64 it goes in
/// memory, as gcc's does, and every other vector goes as its carrier.
#[test]
fn diagnostics_vector_passing_is_refused_only_without_a_carrier() {
    let prelude = "typedef float V1SF __attribute__((vector_size(4)));\n\
                   typedef int V2SI __attribute__((vector_size(8)));\n\
                   long f(); long l; int c;\n";
    let compile_for = |name: &str, body: &str, target: &str| {
        compile(
            name,
            &format!("{prelude}{body}\n"),
            &[&format!("--target={target}")],
        )
    };
    for (name, body) in [
        ("argument", "void t(void) { V1SF v = {1}; f(v); }"),
        ("parameter", "long t(V1SF v) { return 0; }"),
        ("return", "V1SF t(void) { V1SF v = {1}; return v; }"),
    ] {
        let a64 = compile_for(
            &format!("vector_value_{name}"),
            body,
            "aarch64-unknown-linux-gnu",
        );
        assert!(!a64.success, "{name} accepted on aarch64");
        assert!(
            a64.stderr
                .contains("c17 does not pass or return this vector type on this target"),
            "{}",
            a64.stderr
        );
        let x86 = compile_for(
            &format!("vector_value_{name}_x86"),
            body,
            "x86_64-unknown-linux-gnu",
        );
        assert!(x86.success, "{name} on x86-64: {}", x86.stderr);
    }
    compile_expect_ok(
        "vector_value_passed",
        &format!(
            "{prelude}V2SI t(V2SI v) {{ return v; }}\n\
             void u(void) {{ V2SI v = {{1, 2}}; f(v); (void)t(v); }}\n"
        ),
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
            let run = compile(name, src, &[&format!("--target={target}")]);
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
        let run = compile("alias_darwin", src, &[&format!("--target={target}")]);
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
    let run = compile("type_name_vla_member", src, &[]);
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
