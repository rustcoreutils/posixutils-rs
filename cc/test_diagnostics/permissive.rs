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
/// neither the attribute nor the reason. gcc rejects the same program, in
/// these words, whether or not an out-of-line copy exists.
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
        "function 'bad' can never be inlined because it uses variable argument lists",
    );
}

/// The other bodies gcc can never copy are rejected as `always_inline`, in
/// gcc's words, though each has an out-of-line copy to call: gcc splices
/// `always_inline` without exception, so a call it cannot splice is an error
/// rather than a call -- at -O0 as at -O2. An unused one is not.
#[test]
fn diagnostics_always_inline_bodies_gcc_never_copies() {
    let cases = [
        (
            "static inline __attribute__((always_inline)) int cg(int n)\n\
             { void *t[] = { &&a, &&b }; goto *t[n]; a: return 1; b: return 2; }\n\
             int f(int n) { return cg(n); }\n",
            "function 'cg' can never be inlined because it contains a computed goto",
        ),
        (
            "static inline __attribute__((always_inline)) int sl(int n)\n\
             { static void *t[] = { &&a, &&b }; goto *t[n]; a: return 1; b: return 2; }\n\
             int f(int n) { return sl(n); }\n",
            "function 'sl' can never be copied because it saves address of local label in a static variable",
        ),
        (
            "extern void *buf[5];\n\
             static inline __attribute__((always_inline)) int sj(int n)\n\
             { if (__builtin_setjmp(buf)) return 1; return n; }\n\
             int f(int n) { return sj(n); }\n",
            "function 'sj' can never be copied because it receives a non-local goto",
        ),
        (
            "extern long jb[64]; extern int _setjmp(long *);\n\
             static inline __attribute__((always_inline)) int ls(int n)\n\
             { if (_setjmp(jb)) return 1; return n; }\n\
             int f(int n) { return ls(n); }\n",
            "function 'ls' can never be inlined because it uses setjmp",
        ),
        (
            "extern int rt(void *) __attribute__((returns_twice)); extern long jb[64];\n\
             static inline __attribute__((always_inline)) int at(int n)\n\
             { if (rt(jb)) return 1; return n; }\n\
             int f(int n) { return at(n); }\n",
            "function 'at' can never be inlined because it uses setjmp",
        ),
    ];
    for (src, expected) in cases {
        for opt in ["-O0", "-O2"] {
            let run = compile("diag_always_inline_never_copied", src, &[opt]);
            assert!(
                !run.success && run.stderr.contains(expected),
                "{opt}: expected {expected:?}:\n{}",
                run.stderr
            );
        }
    }
    compile_expect_ok(
        "diag_always_inline_never_copied_unused",
        "static inline __attribute__((always_inline)) int cg(int n)\n\
         { void *t[] = { &&a, &&b }; goto *t[n]; a: return 1; b: return 2; }\n\
         int f(int n) { return n; }\n",
    );
}

/// Recursion among `always_inline` functions, which gcc can never finish
/// splicing, with gcc 13's verdict at each level.
///
/// A cycle through two or more of them is rejected at every level, once,
/// naming the first one defined, wherever it is called from -- even when only
/// its address is taken -- unless nothing keeps the cycle at all. A function
/// that calls itself is rejected at -O0 when another function calls it, the
/// call gcc splices while leaving the recursive one inside the copy; at -O1
/// and -Og whenever it is kept. (From -O2 gcc's verdict turns on whether its
/// tail-recursion pass removed the call first, which c17 has no pass to
/// match: it accepts there.)
#[test]
fn diagnostics_always_inline_recursion() {
    const AI: &str = "static inline __attribute__((always_inline))";
    let cycle = format!(
        "{AI} int b(int n);\n\
         {AI} int a(int n) {{ return n ? b(n - 1) + 1 : 0; }}\n\
         {AI} int b(int n) {{ return n ? a(n - 1) + 2 : 0; }}\n"
    );
    let reversed = format!(
        "{AI} int a(int n);\n\
         {AI} int b(int n) {{ return n ? a(n - 1) + 2 : 0; }}\n\
         {AI} int a(int n) {{ return n ? b(n - 1) + 1 : 0; }}\n"
    );
    let three = format!(
        "{AI} int c(int n);\n{AI} int b(int n);\n\
         {AI} int a(int n) {{ return n ? b(n - 1) + 1 : 0; }}\n\
         {AI} int b(int n) {{ return n ? c(n - 1) + 2 : 0; }}\n\
         {AI} int c(int n) {{ return n ? a(n - 1) + 3 : 0; }}\n"
    );
    let selfrec = format!("{AI} int f(int n) {{ return n ? f(n - 1) + 1 : 0; }}\n");
    let not_inlinable = |f: &str| {
        format!("inlining failed in call to 'always_inline' '{f}': function not inlinable")
    };
    let not_considered =
        "inlining failed in call to 'always_inline' 'f': function not considered for inlining";
    let recursive = "inlining failed in call to 'always_inline' 'f': recursive inlining";
    let cases: Vec<(String, [Option<String>; 3])> = vec![
        (
            format!("{cycle}int g(int n) {{ return a(n); }}\n"),
            [0, 1, 2].map(|_| Some(not_inlinable("a"))),
        ),
        (
            format!("{cycle}int g(int n) {{ return b(n); }}\n"),
            [0, 1, 2].map(|_| Some(not_inlinable("a"))),
        ),
        (
            format!("{reversed}int g(int n) {{ return a(n); }}\n"),
            [0, 1, 2].map(|_| Some(not_inlinable("b"))),
        ),
        (
            format!("{three}int g(int n) {{ return a(n); }}\n"),
            [0, 1, 2].map(|_| Some(not_inlinable("a"))),
        ),
        (
            format!("{cycle}int (*p)(int) = a;\n"),
            [0, 1, 2].map(|_| Some(not_inlinable("a"))),
        ),
        (
            format!("{cycle}int g(int n) {{ return n; }}\n"),
            [None, None, None],
        ),
        (
            format!("{selfrec}int g(int n) {{ return f(n); }}\n"),
            [
                Some(not_considered.to_string()),
                Some(recursive.to_string()),
                None,
            ],
        ),
        (
            format!("{selfrec}int (*p)(int) = f;\n"),
            [None, Some(recursive.to_string()), None],
        ),
        (
            format!("{selfrec}int g(int n) {{ return n; }}\n"),
            [None, None, None],
        ),
    ];
    for (src, verdicts) in &cases {
        for (opt, verdict) in ["-O0", "-O1", "-O2"].iter().zip(verdicts) {
            let run = compile("diag_always_inline_recursion", src, &[opt]);
            match verdict {
                Some(msg) => assert!(
                    !run.success && run.stderr.contains(msg.as_str()),
                    "{opt}: expected {msg:?} for\n{src}\ngot:\n{}",
                    run.stderr
                ),
                None => assert!(
                    run.success && !run.stderr.contains("inlining failed"),
                    "{opt}: expected no error for\n{src}\ngot:\n{}",
                    run.stderr
                ),
            }
        }
    }
    // `-Og` is gcc's -O1 here.
    let run = compile(
        "diag_always_inline_recursion_og",
        &format!("{selfrec}int (*p)(int) = f;\n"),
        &["-Og"],
    );
    assert!(
        !run.success && run.stderr.contains(recursive),
        "{}",
        run.stderr
    );
}

// ============================================================================
// What `-fpermissive` relaxes
// ============================================================================

/// The constraints GCC 13 only warns about and GCC 14 refuses, which c17
/// relaxes only when asked.
///
/// Each is a genuine C17 constraint violation, and each appears in source old
/// enough that gcc once chose to warn rather than refuse. `-fpermissive` is
/// where c17 keeps that leniency: it already covers implicit `int` and
/// implicit function declarations, and these join them. The constraints gcc
/// still only warns about are warnings here too (`default_pedwarns.rs`).
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

/// An automatic object past two gigabytes is compiled, on both targets, as
/// gcc compiles it: C sets no bound of its own here, and the frame's is
/// `PTRDIFF_MAX` less what the prologue adds
/// (`TypeTable::MAX_FRAME_BYTES`). Every displacement past 32 bits is
/// reached through a register.
///
/// Until frame displacements were widened this bound was `i32::MAX` and
/// every case below was refused; before that there was no diagnostic at all,
/// and `char a[3000000000];` in a function emitted `subq $32, %rsp` with the
/// array at `leaq -40(%rbp)` on x86-64, because the byte count cast `as i32`
/// wrapped to -1294967296 and the `size.max(8)` that follows gave it eight
/// bytes. So each is also checked for the `movabsq` that reaches it.
///
/// The cases that are not declarations are asked somewhere other than the
/// declarator loop: a compound literal has automatic storage duration by
/// C17 6.5.2.5p5, a K&R parameter's real type arrives after the identifier
/// list, and an aggregate *return* type is not an object at all -- that one
/// reaches `abi::slot_bytes` in the backend, which is why its refusal past
/// the bound reads differently.
#[test]
fn diagnostics_stack_object_past_two_gigabytes_is_accepted() {
    let cases = [
        (
            "automatic_array",
            "extern void sink(char *);\n\
             int f(void){ char a[3000000000]; a[0]=1; sink(a); return a[0]; }\n",
        ),
        (
            "automatic_array_of_int",
            "int f(void){ int a[600000000]; a[0]=1; return a[0]; }\n",
        ),
        (
            "automatic_struct",
            "struct S { char x[3000000000]; };\n\
             int f(void){ struct S s; s.x[0]=1; return s.x[0]; }\n",
        ),
        (
            "automatic_register",
            "int f(void){ register char a[3000000000]; return a[0]; }\n",
        ),
        (
            "automatic_nested_block",
            "int f(int c){ if (c) { char a[3000000000]; return a[0]; } return 0; }\n",
        ),
        (
            "parameter_definition",
            "struct S { char x[3000000000]; };\n\
             int f(struct S s){ return s.x[0]; }\n",
        ),
        (
            "compound_literal",
            "struct S { char x[3000000000]; };\nvoid sink(struct S *);\n\
             void f(void){ sink(&(struct S){0}); }\n",
        ),
        (
            "parameter_knr",
            "struct S { char x[3000000000]; };\n\
             int f(a) struct S a; { return a.x[0]; }\n",
        ),
        (
            "aggregate_return_temporary",
            "struct S { char x[3000000000]; };\nextern struct S g(void);\n\
             int f(void){ return g().x[0]; }\n",
        ),
    ];
    for (name, src) in cases {
        for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
            let run = compile(name, src, &[&format!("--target={target}")]);
            assert!(run.success, "{name} on {target}:\n{}", run.stderr);
            let asm = run.asm.unwrap_or_default();
            let far = if target.starts_with("x86_64") {
                "movabsq $"
            } else {
                "movk x15, "
            };
            assert!(asm.contains(far), "{name} on {target}: no {far}\n{asm}");
        }
    }
    // Declared, and not defined: no frame at all.
    compile_expect_ok(
        "parameter_prototype",
        "struct S { char x[3000000000]; };\nint f(struct S s);\nvoid g(struct S);\n",
    );
}

/// What no frame can hold is still refused, where it is declared when it is
/// declared: an object of `PTRDIFF_MAX` bytes, which C can describe, and an
/// aggregate return temporary of that size, which only the backend sees.
#[test]
fn diagnostics_stack_object_past_the_frame_bound_is_rejected() {
    for (name, src, expected) in [
        (
            "automatic_array",
            "extern void sink(char *);\n\
             void f(void){ char a[0x7fffffffffffffff]; sink(a); }\n",
            "maximum stack object size",
        ),
        (
            "parameter_prototype",
            "struct S { char x[0x7fffffffffffffff]; };\nint f(struct S s);\n",
            "maximum stack object size",
        ),
        (
            "aggregate_return_temporary",
            "struct S { char x[0x7fffffffffffffff]; };\nextern struct S g(void);\n\
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

/// Every vector a declaration admits is passed and returned: gcc refuses a
/// lane count that is not a power of two where the type is declared, so
/// each size left is a register width or an aggregate. A one-lane `float`
/// vector goes on the stack on aarch64 and in memory on x86-64, as gcc
/// passes it, and a vector whose alignment a typedef raises travels too --
/// it had no carrier, and was refused.
#[test]
fn diagnostics_every_declared_vector_is_passed() {
    let prelude = "typedef float V1SF __attribute__((vector_size(4)));\n\
                   typedef int V8SI __attribute__((vector_size(32)));\n\
                   typedef V8SI W8SI __attribute__((aligned(64)));\n\
                   typedef V1SF W1SF __attribute__((aligned(16)));\n\
                   typedef int V2SI __attribute__((vector_size(8)));\n\
                   long f(); long l; int c;\n";
    let compile_for = |name: &str, body: &str, target: &str| {
        compile(
            name,
            &format!("{prelude}{body}\n"),
            &[&format!("--target={target}")],
        )
    };
    for (name, shape) in [
        ("argument", "void t(void) { V v = {1}; f(v); }"),
        ("parameter", "long t(V v) { return 0; }"),
        ("return", "V t(void) { V v = {1}; return v; }"),
    ] {
        for target in [
            "aarch64-unknown-linux-gnu",
            "aarch64-apple-darwin",
            "x86_64-unknown-linux-gnu",
        ] {
            for vector in ["V1SF", "W1SF", "W8SI"] {
                let passed = compile_for(
                    &format!("vector_value_{name}_{vector}"),
                    &shape.replace('V', vector),
                    target,
                );
                assert!(
                    passed.success,
                    "{name} of {vector} on {target}: {}",
                    passed.stderr
                );
            }
        }
    }
    compile_expect_ok(
        "vector_value_passed",
        &format!(
            "{prelude}V2SI t(V2SI v) {{ return v; }}\n\
             void u(void) {{ V2SI v = {{1, 2}}; f(v); (void)t(v); }}\n"
        ),
    );
}

/// Frames at the edge of the old 32-bit ceiling, on both targets: compiled
/// whole, and frames past `PTRDIFF_MAX` refused, never wrapped.
///
/// Each of the first five was accepted once and came out wrong, because the
/// ceiling was `i32::MAX` and the arithmetic after it -- a slot's alignment
/// rounding, the prologue's saved registers and variadic save area, the final
/// frame rounding -- ran past it in `i32`:
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
///
/// Then they were refused; frame quantities are `i64` now, so they compile.
/// The last two pass the `i64` bound itself: one object, rounded to its
/// alignment, and two objects whose sum overflows `i64`.
#[test]
fn diagnostics_frame_at_the_old_ceiling_compiles_and_past_ptrdiff_max_is_refused() {
    let compiles = [
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
    let refused = [
        (
            "aligned_past",
            "extern void sink(void *);\n\
             void f(void){ _Alignas(64) char a[0x7fffffffffffefc0]; sink(a); }\n",
        ),
        (
            "sum_past",
            "extern void sink(void *);\n\
             void f(void){ char a[1L << 62], b[1L << 62]; sink(a); sink(b); }\n",
        ),
    ];
    for target in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        let flag = format!("--target={target}");
        for (name, src) in compiles {
            let run = compile(name, src, &[&flag]);
            assert!(run.success, "{name} on {target}:\n{}", run.stderr);
        }
        for (name, src) in refused {
            let run = compile(name, src, &[&flag]);
            assert!(!run.success, "{name} on {target} compiled:\n{}", run.stderr);
            assert!(
                run.stderr.contains("stack frame"),
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
