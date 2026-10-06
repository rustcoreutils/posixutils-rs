use crate::test_compile::{
    compile, compile_expect_error, compile_expect_ok, compile_expect_warning, compile_rejected,
};

// ============================================================================
// #C116 — the lock-free atomic ceiling
// ============================================================================

/// #C123: a constraint violation inside an abstract declarator must be
/// reported as itself, not discarded.
///
/// `try_parse_type_name_vm` backtracks by restoring the token cursor, and its
/// fallback arm collapsed two situations: "the declarator produced a name, so
/// this was never a type-name" and "this *is* a type-name and its declarator
/// is invalid". The second rewound too, so the real error was dropped and the
/// caller re-read `char[-1]` as a subscript expression -- producing
/// "undeclared identifier 'char'" and "subscripted value is neither array nor
/// pointer", two diagnostics about neither problem.
///
/// Every constraint an abstract declarator can violate was reported that way.
/// The wording is gcc's, including the named/unnamed distinction.
#[test]
fn diagnostics_abstract_declarator_reports_its_own_error() {
    for (name, src) in [
        (
            "abstract_neg_array_sizeof",
            "int main(void){ return sizeof(char[-1]); }\n",
        ),
        (
            "abstract_neg_array_alignof",
            "int main(void){ return _Alignof(char[-1]); }\n",
        ),
        (
            "abstract_neg_array_cast",
            "int main(void){ return (int)(char(*)[-1])0; }\n",
        ),
    ] {
        compile_expect_error(name, src, "size of unnamed array is negative");
    }

    // A declarator that *has* a name keeps naming it, as gcc does.
    compile_expect_error(
        "named_neg_array",
        "int main(void){ char a[-1]; return 0; }\n",
        "size of array 'a' is negative",
    );
}

/// The discarded diagnostic is only half the defect: the fallback's two
/// *spurious* messages were the visible half, and they must be gone.
#[test]
fn diagnostics_abstract_declarator_does_not_cascade() {
    let run = compile(
        "abstract_no_cascade",
        "int main(void){ return sizeof(char[-1]); }\n",
        &[],
    );

    assert!(!run.success, "the program must still be rejected");
    for spurious in ["undeclared identifier", "subscripted value", "expected ')'"] {
        assert!(
            !run.stderr.contains(spurious),
            "the fallback parse still leaks {:?}:\n{}",
            spurious,
            run.stderr
        );
    }
    assert_eq!(
        run.stderr.matches("error:").count(),
        1,
        "one bad declarator must draw exactly one error:\n{}",
        run.stderr
    );
}

/// Committing to the type-name reading must not swallow the *expression*
/// reading, which is what the rewind is legitimately for: `(x)` in a cast
/// position is a parenthesized identifier, and `sizeof(x)` is sizeof an
/// object. Without these the test above would pass against a parser that had
/// simply stopped backtracking.
#[test]
fn diagnostics_type_name_backtracking_still_works() {
    compile_expect_ok(
        "backtrack_paren_expr",
        "int main(void){ int x = 3; return (x) - 3; }\n",
    );
    compile_expect_ok(
        "backtrack_sizeof_object",
        "int main(void){ int x = 0; (void)x; return sizeof(x) - sizeof(int); }\n",
    );
    compile_expect_ok(
        "backtrack_cast_to_ptr_to_array",
        "int a[3]; int main(void){ int (*p)[3] = (int(*)[3])&a; return (*p)[0]; }\n",
    );
    compile_expect_ok(
        "backtrack_compound_literal",
        "struct S { int a; };\nint main(void){ return ((struct S){0}).a; }\n",
    );
}

/// #C125: a shift whose constant count cannot name a bit of the value being
/// shifted draws a diagnostic, as gcc's does.
///
/// C17 6.5.7p3 makes such a shift undefined, and c17's answer -- the count
/// masked the way the hardware masks it -- is as defensible as gcc's, which is
/// not even self-consistent (`1 << 64` folds to 0 while `-1 >> 64` stays -1).
/// The gap was never the value; it was the silence.
///
/// The width is the *promoted left* operand's, so `(char)1 << 40` warns (char
/// promotes to int) and `1L << 63` does not. Only the count need be constant:
/// `x << 64` warns. Every row here was taken from `gcc -std=c17`.
#[test]
fn diagnostics_shift_count_out_of_range_warns() {
    for (name, src, expected) in [
        (
            "shift_left_64",
            "int main(void){ return 1 << 64; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_left_32",
            "int main(void){ return 1 << 32; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_right_64",
            "int main(void){ return 1 >> 64; }\n",
            "right shift count >= width of type",
        ),
        (
            "shift_long_64",
            "int main(void){ return (int)(1L << 64); }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_char_40",
            "int main(void){ return (char)1 << 40; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_var_left",
            "int x = 1;\nint main(void){ return x << 64; }\n",
            "left shift count >= width of type",
        ),
        (
            "shift_negative",
            "int main(void){ return 1 << -1; }\n",
            "left shift count is negative",
        ),
    ] {
        compile_expect_warning(name, src, expected);
    }
}

/// The accept side, which is what keeps the check from being "warn on every
/// shift": a count inside the promoted left operand's width is silent, and so
/// is a count that is not a constant at all.
#[test]
fn diagnostics_shift_count_in_range_is_silent() {
    for (name, src) in [
        (
            "shift_left_31_ok",
            "int main(void){ return (1 << 31) != 0; }\n",
        ),
        (
            "shift_long_63_ok",
            "int main(void){ return (int)((1L << 63) != 0); }\n",
        ),
        ("shift_zero_ok", "int main(void){ return 1 << 0; }\n"),
        (
            "shift_var_count_ok",
            "int n = 3;\nint main(void){ return (1 << n) - 8; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

/// A shift by a negative count is no constant expression: gcc warns and then
/// refuses it wherever C requires one -- a static initializer at file or
/// block scope, an enumerator, a `case` label, `_Static_assert`, a file-scope
/// array size -- in these words. A count past the width is constant (it only
/// warns), and so is a negative value shifted.
#[test]
fn diagnostics_negative_shift_count_is_not_constant() {
    const INIT: &str = "initializer element is not constant";
    for (name, src, expected) in [
        ("negshift_file", "int x = 1 << -1;\n", INIT),
        ("negshift_right_file", "int x = 1 >> -1;\n", INIT),
        ("negshift_long_file", "long x = 1L >> -3;\n", INIT),
        ("negshift_expr_file", "int x = 1 << (0 - 1);\n", INIT),
        ("negshift_static_file", "static int x = 1 << -1;\n", INIT),
        (
            "negshift_static_block",
            "int f(void) { static int x = 1 << -1; return x; }\n",
            INIT,
        ),
        (
            "negshift_enum",
            "enum { E = 1 << -1 };\n",
            "enumerator value for 'E' is not an integer constant",
        ),
        (
            "negshift_static_assert",
            "_Static_assert((1 << -1) || 1, \"\");\n",
            "expression in static assertion is not constant",
        ),
        (
            "negshift_case",
            "int f(int v) { switch (v) { case 1 << -1: return 1; } return 0; }\n",
            "case label is not an integer constant expression",
        ),
        (
            "negshift_array_file",
            "int a[1 << -1 ? 1 : 2];\n",
            "variable length arrays cannot have file scope",
        ),
    ] {
        let stderr = compile_rejected(name, src);
        assert!(
            stderr.contains("shift count is negative") && stderr.contains(expected),
            "'{name}': expected the warning and {expected:?}.\nstderr:\n{stderr}"
        );
    }
}

/// The accept side: the same shift where no constant is required, a count
/// past the width, a negative value shifted, and a negative count that is
/// never evaluated.
#[test]
fn diagnostics_negative_shift_count_where_no_constant_is_needed() {
    for (name, src) in [
        (
            "negshift_auto_ok",
            "int f(void) { int x = 1 << -1; return x; }\n",
        ),
        ("bigshift_file_ok", "int x = 1 << 40;\n"),
        ("bigshift_enum_ok", "enum { E = 1 << 40 };\n"),
        ("bigshift_right_file_ok", "int x = 1 >> 40;\n"),
        ("negvalue_file_ok", "int x = -1 << 1;\nint y = -1 >> 1;\n"),
        ("negvalue_enum_ok", "enum { E = -1 << 1 };\n"),
        (
            "negvalue_static_block_ok",
            "int f(void) { static int x = -1 << 1; return x; }\n",
        ),
        ("negshift_unevaluated_ok", "int x = 0 ? 1 << -1 : 2;\n"),
        (
            "negshift_short_circuit_ok",
            "int x = 1 || (1 << -1);\nint y = 0 && (1 << -1);\n",
        ),
        ("negshift_sizeof_ok", "int x = sizeof(1 << -1);\n"),
    ] {
        compile_expect_ok(name, src);
    }
}

/// `-Wno-shift-count-overflow` and `-Wno-shift-count-negative` turn the two
/// groups off separately, as gcc spells them.
#[test]
fn diagnostics_shift_count_warnings_can_be_turned_off() {
    for (name, src, flag) in [
        (
            "shift_off_overflow",
            "int main(void){ return 1 << 64; }\n",
            "-Wno-shift-count-overflow",
        ),
        (
            "shift_off_negative",
            "int main(void){ return 1 << -1; }\n",
            "-Wno-shift-count-negative",
        ),
    ] {
        let run = compile(name, src, &[flag]);
        assert!(run.success, "{} should still compile: {}", name, run.stderr);
        assert!(
            !run.stderr.contains("shift count"),
            "{} did not silence the warning:\n{}",
            flag,
            run.stderr
        );
    }
}

/// #C132: a diagnostic must name the type the source could have written.
///
/// `int m[4][8]; int *p = m;` reported the pointee as `int[8] *`, which reads
/// as "array of pointers" -- the other type entirely. `format_type` built the
/// spelling left to right and had no notion of a declarator's inside-out
/// reading, so it could not parenthesize. gcc says `int (*)[8]`.
#[test]
fn diagnostics_pointer_to_array_is_spelled_as_a_declarator() {
    let run = compile("spell_ptr_to_array", "int m[4][8];\nint *p = m;\n", &[]);

    assert!(
        run.stderr.contains("int (*)[8]"),
        "expected the declarator spelling gcc uses, got:\n{}",
        run.stderr
    );
    assert!(
        !run.stderr.contains("int[8] *"),
        "the suffix spelling names a different type:\n{}",
        run.stderr
    );
}

/// The composition, through a diagnostic rather than the type table directly:
/// a pointer to a function and an array of pointers must not collapse into
/// each other's spelling.
#[test]
fn diagnostics_function_pointer_is_spelled_as_a_declarator() {
    let run = compile(
        "spell_fn_ptr",
        "int f(void);\nint (*fp)(void) = f;\nint bad = fp;\n",
        &[],
    );

    assert!(
        run.stderr.contains("int (*)(void)"),
        "expected `int (*)(void)`, got:\n{}",
        run.stderr
    );
}

/// Only one x87 asm output, because only one can be written back.
///
/// The write-back is a single slot; a second output would overwrite it and the
/// first result would be dropped, with the FP stack depth no longer matching
/// what the template left behind.
#[test]
#[cfg(target_arch = "x86_64")]
fn diagnostics_one_x87_asm_output() {
    compile_expect_error(
        "x87_two_outputs",
        "struct P { long double a, b; };\n\
         void g(struct P *p){ __asm__(\"fldz\\nfld1\" : \"=t\"(p->a), \"=u\"(p->b)); }\n\
         int main(void){ struct P p; g(&p); return 0; }\n",
        "x87 asm output",
    );
}

/// A `vector_size` beyond the maximum object size is refused.
///
/// This interns an array type directly, so nothing else would catch an absurd
/// width: `vector_size` once quietly produced a four-gigabyte type, and a
/// width near `u64::MAX` overflowed `next_power_of_two` -- a panic in a debug
/// build. The bound moved with the object-size limit; what this pins is that
/// the check is still reached.
#[test]
fn diagnostics_vector_size_is_bounded() {
    compile_expect_error(
        "vector_size_too_big",
        "typedef float V __attribute__((vector_size(9223372036854775808UL)));\n\
         int main(void){ return 0; }\n",
        "'vector_size' attribute argument value '9223372036854775808' exceeds \
         9223372036854775807",
    );
}

/// A `vector_size` declaration gcc refuses is refused in gcc's words: a
/// width that is zero, negative or no whole number of lanes, a lane count
/// that is not a power of two, and a lane that is complex or `_Bool`. The
/// lane count is what makes every vector a convention has to pass one of
/// its register or memory widths.
#[test]
fn diagnostics_vector_size_shapes_gcc_refuses() {
    for (name, decl, expected) in [
        (
            "zero",
            "short V __attribute__((vector_size(0)))",
            "zero vector size",
        ),
        (
            "negative",
            "float V __attribute__((vector_size(-16)))",
            "'vector_size' attribute argument value '-16' is negative",
        ),
        (
            "not_multiple",
            "short V __attribute__((vector_size(5)))",
            "vector size not an integral multiple of component size",
        ),
        (
            "three_shorts",
            "short V __attribute__((vector_size(6)))",
            "number of vector components 3 not a power of two",
        ),
        (
            "three_chars",
            "char V __attribute__((vector_size(3)))",
            "number of vector components 3 not a power of two",
        ),
        (
            "three_doubles",
            "double V __attribute__((vector_size(24)))",
            "number of vector components 3 not a power of two",
        ),
        (
            "many_ints",
            "int V __attribute__((vector_size(1536)))",
            "number of vector components 384 not a power of two",
        ),
        (
            "complex",
            "_Complex float V __attribute__((vector_size(16)))",
            "invalid vector type for attribute 'vector_size'",
        ),
        (
            "bool",
            "_Bool V __attribute__((vector_size(4)))",
            "invalid vector type for attribute 'vector_size'",
        ),
    ] {
        for form in ["typedef {D};\n", "{D} obj;\n", "struct S { {D}; };\n"] {
            compile_expect_error(
                &format!("vector_size_{name}"),
                &format!(
                    "{}int main(void){{ return 0; }}\n",
                    form.replace("{D}", decl)
                ),
                expected,
            );
        }
    }
    compile_expect_ok(
        "vector_size_powers_of_two",
        "typedef char V1 __attribute__((vector_size(1)));\n\
         typedef short V2 __attribute__((vector_size(4)));\n\
         typedef double V4 __attribute__((vector_size(32)));\n\
         typedef int V64 __attribute__((vector_size(256)));\n\
         _Static_assert(sizeof(V1) + sizeof(V2) + sizeof(V4) + sizeof(V64) == 293, \"\");\n",
    );
}

/// An attribute's integer argument is an integer constant expression, and
/// one that is not constant, or not a usable value, is refused in gcc's words
/// rather than dropped: the attribute parser once read a single token, so
/// `aligned(x)` and `aligned(3)` both left the object silently unaligned.
#[test]
fn diagnostics_attribute_integer_arguments() {
    let main = "int main(void){ return 0; }\n";
    for (name, decl, expected) in [
        (
            "aligned_not_constant",
            "int x; char a __attribute__((aligned(x)));",
            "requested alignment is not an integer constant",
        ),
        (
            "aligned_string",
            "char a __attribute__((aligned(\"s\")));",
            "requested alignment is not an integer constant",
        ),
        (
            "aligned_not_power_of_two",
            "char a __attribute__((aligned(3)));",
            "requested alignment '3' is not a positive power of 2",
        ),
        (
            "aligned_negative",
            "char a __attribute__((aligned(-4)));",
            "requested alignment '-4' is not a positive power of 2",
        ),
        (
            "aligned_too_large",
            "char a __attribute__((aligned(1ULL << 40)));",
            "exceeds object file maximum",
        ),
        (
            "aligned_two_arguments",
            "char a __attribute__((aligned(16, 32)));",
            "wrong number of arguments specified for 'aligned' attribute",
        ),
        (
            "alignas_not_constant",
            "int x; _Alignas(x) char a;",
            "requested alignment is not an integer constant",
        ),
        (
            "vector_size_not_constant",
            "int x; typedef int V __attribute__((vector_size(x)));",
            "'vector_size' attribute argument is not an integer constant",
        ),
        (
            "constructor_priority_not_constant",
            "int x; void f(void) __attribute__((constructor(x)));",
            "constructor priorities must be integers from 0 to 65535 inclusive",
        ),
        (
            "constructor_priority_out_of_range",
            "void f(void) __attribute__((constructor(70000)));",
            "constructor priorities must be integers from 0 to 65535 inclusive",
        ),
    ] {
        compile_expect_error(name, &format!("{decl}\n{main}"), expected);
    }
    // gcc warns about, and ignores, `aligned(0)`; and an attribute c17 does
    // not act on is dropped with a warning, not refused.
    compile_expect_warning(
        "aligned_zero",
        &format!("char a __attribute__((aligned(0)));\n{main}"),
        "requested alignment '0' is not a positive power of 2",
    );
    compile_expect_warning(
        "alloc_size_not_constant",
        &format!("int x; void *m(int) __attribute__((alloc_size(x)));\n{main}"),
        "'alloc_size' attribute argument is not an integer constant",
    );
    // Names stay names where the attribute wants one.
    compile_expect_ok(
        "attribute_names_and_strings",
        &format!(
            "enum {{ I = 1 }};\n\
             int pf(const char *, ...) __attribute__((__format__(__printf__, I, I + 1)));\n\
             typedef int SI __attribute__((mode(SI)));\n\
             char s __attribute__((section(\"a\" \"b\"), aligned));\n\
             void *m(int) __attribute__((alloc_size(I), malloc));\n{main}"
        ),
    );
}

/// A decimal constant too large for any type draws gcc's one warning,
/// "integer constant is too large for its type"; c17 added "so large that
/// it is unsigned" about the truncated value.
#[test]
fn too_large_decimal_constant_warns_once() {
    let out = crate::test_compile::compile_accepted(
        "too_large_decimal",
        "unsigned long long x = 123456789012345678901234567890;\n",
        &[],
    );
    assert_eq!(
        out.matches("integer constant is too large for its type")
            .count(),
        1,
        "{out}"
    );
    assert!(!out.contains("so large that it is unsigned"), "{out}");
}
