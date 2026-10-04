//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Negative-path tests: programs that must be REJECTED.
//
// Every other suite proves that accepted programs run correctly. None proved
// that invalid programs are diagnosed — `compile_and_run` collapses a compile
// failure into the sentinel `-1` and discards stderr — which is how a dozen
// missing C constraint checks went unnoticed.
//
// Each constraint gets both directions: a program that must be rejected, and
// one that must still be accepted, so a check cannot pass by rejecting
// everything.
//

mod cast_to_union;
mod constraint_sweep;
mod declarations;
mod expressions;
mod function_compatibility;

use crate::common::{
    compile_and_run, compile_and_run_two_units, compile_expect_error, compile_expect_ok,
    create_c_file, run_c17,
};

// ============================================================================
// #C56 — `void *` against a function pointer
// ============================================================================

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
    let c = create_c_file("fnptr_no_over_fire", src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
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
    let c = create_c_file("fnptr_silence", src);
    let path = c.path().to_string_lossy().to_string();

    for silencer in ["-w", "-Wno-function-pointer-conv"] {
        let run = run_c17(&["-S", "-o", "/dev/null", silencer, &path]);
        assert!(run.success, "{silencer} should be accepted: {}", run.stderr);
        assert!(
            !run.stderr.contains("ISO C forbids"),
            "{silencer} should silence the conversion warning, got:\n{}",
            run.stderr
        );
    }

    // An unrelated -Wno- must not silence it, or the flag name means nothing.
    let run = run_c17(&["-S", "-o", "/dev/null", "-Wno-unused", &path]);
    assert!(
        run.stderr.contains("ISO C forbids"),
        "-Wno-unused should leave it alone, got:\n{}",
        run.stderr
    );
}

/// ...and it belongs to the `attributes` group, like every other
/// unimplemented-or-ignored attribute diagnostic.
#[test]
fn diagnostics_transparent_union_warning_can_be_silenced() {
    let src = "struct S { int a; } __attribute__((transparent_union));\nstruct S x;\n";
    let c = create_c_file("transparent_union_silence", src);
    let path = c.path().to_string_lossy().to_string();
    for silencer in ["-w", "-Wno-attributes"] {
        let run = run_c17(&["-S", "-o", "/dev/null", silencer, &path]);
        assert!(run.success, "{silencer} should be accepted: {}", run.stderr);
        assert!(
            !run.stderr.contains("transparent_union"),
            "{silencer} should silence it, got:\n{}",
            run.stderr
        );
    }
}

// ============================================================================
// Jumping into a GNU statement expression
// ============================================================================

/// The `error:` lines c17 prints for `src`, which must fail to compile.
fn compile_errors(name: &str, src: &str) -> Vec<String> {
    let c_file = create_c_file(name, src);
    let path = c_file.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
    assert!(!run.success, "{name}: expected a compile error");
    run.stderr
        .lines()
        .filter(|l| l.contains("error:"))
        .map(str::to_string)
        .collect()
}

/// gcc forbids entering a statement expression by `goto`, by `asm goto` or by
/// a `switch` reaching a `case` or `default` inside one: control would arrive
/// in the middle of evaluating the expression around it. c17 accepted every
/// form and generated the jump. Each is one error, in gcc's words, as many
/// times as gcc gives it.
#[test]
fn diagnostics_jump_into_statement_expression_is_rejected() {
    for (name, src, count) in [
        ("se_goto", "int f(int x) { goto L; return ({ L: x; }); }", 1),
        (
            "se_goto_sibling",
            "int f(void) { int a = ({ goto N; 1; }); int b = ({ N: 2; }); return a + b; }",
            1,
        ),
        (
            "se_goto_nested",
            "int f(void) { return ({ goto L; ({ L: 1; }); }); }",
            1,
        ),
        (
            "se_goto_inner_block",
            "int f(int x) { goto L; return ({ { L: x; } 1; }); }",
            1,
        ),
        (
            "se_goto_twice",
            "int f(int x) { goto L; ({ L: x; }); ({ M: x; }); goto M; return 0; }",
            2,
        ),
        (
            "se_asm_goto",
            "int f(int x) { asm goto (\"\" :::: R); return ({ R: x; }); }",
            1,
        ),
        // A label in an operand that is never evaluated is still written, so
        // the jump is the one error, as gcc has it: lowering never placed the
        // label and added "label 'L' used but not defined".
        (
            "se_goto_into_sizeof",
            "int f(int x) { goto L; return sizeof(({ L: x; })); }",
            1,
        ),
        (
            "se_goto_into_alignof",
            "int f(int x) { goto L; return _Alignof(({ L: x; })); }",
            1,
        ),
    ] {
        let errors = compile_errors(name, &format!("{src}\n"));
        assert_eq!(errors.len(), count, "{name}: {errors:?}");
        assert!(
            errors
                .iter()
                .all(|e| e.ends_with("error: jump into statement expression")),
            "{name}: {errors:?}"
        );
    }
}

#[test]
fn diagnostics_switch_into_statement_expression_is_rejected() {
    for (name, src, count) in [
        (
            "se_case",
            "int f(int x) { switch (x) { case 0: return ({ case 1: x; }); } return 0; }",
            1,
        ),
        (
            "se_default",
            "int f(int x) { switch (x) { case 0: return ({ default: x; }); } return 0; }",
            1,
        ),
        // At the top level of the switch body.
        (
            "se_case_top",
            "int f(int x) { switch (x) { ({ case 1: x++; }); } return x; }",
            1,
        ),
        // In a condition, and in an initializer inside a block.
        (
            "se_case_in_if",
            "int f(int x) { switch (x) { case 1: if (({ default: x; })) return 1; } return x; }",
            1,
        ),
        (
            "se_case_in_init",
            "int f(int x) { switch (x) { case 1: { int y = ({ case 3: x; }); return y; } } return x; }",
            1,
        ),
        // Past a switch nested inside the statement expression.
        (
            "se_case_past_inner_switch",
            "int f(int x) { switch (x) { case 0: x = ({ switch (x) { case 1: x; } case 2: 5; }); } return x; }",
            1,
        ),
        // Once per label.
        (
            "se_two_cases",
            "int f(int x) { switch (x) { case 0: x = ({ case 1: x; case 2: x; }); } return x; }",
            2,
        ),
    ] {
        let errors = compile_errors(name, &format!("{src}\n"));
        assert_eq!(errors.len(), count, "{name}: {errors:?}");
        assert!(
            errors
                .iter()
                .all(|e| e.ends_with("error: switch jumps into statement expression")),
            "{name}: {errors:?}"
        );
    }
}

/// Now that the check sees inside statement expressions, the rules it already
/// enforced reach there too: a label name is unique in its function, and a
/// loop's controlling expressions are not inside the loop.
#[test]
fn diagnostics_jump_rules_reach_inside_statement_expressions() {
    compile_expect_error(
        "se_duplicate_label",
        "int f(int x) { ({ L: x; }); ({ L: x; }); return 0; }\n",
        "duplicate label 'L'",
    );
    compile_expect_error(
        "se_break_in_while_cond",
        "int f(int x) { while (({ if (x) break; 1; })) x--; return x; }\n",
        "break statement not within loop or switch",
    );
    compile_expect_error(
        "se_continue_in_do_cond",
        "int f(int x) { do x--; while (({ if (x) continue; 1; })); return x; }\n",
        "continue statement not within a loop",
    );
    compile_expect_error(
        "se_break_in_for_step",
        "int f(int x) { for (;; ({ if (x) break; 1; })) x--; return x; }\n",
        "break statement not within loop or switch",
    );
    compile_expect_error(
        "se_case_in_switch_expr",
        "int f(int x) { switch (({ case 1: x; })) { case 2: ; } return x; }\n",
        "case label not within a switch statement",
    );
    // A variably modified scope inside a statement expression is both.
    let errors = compile_errors(
        "se_vla",
        "int f(int n) { goto L; ({ int a[n]; L: a[0]; }); return 0; }\n",
    );
    assert_eq!(errors.len(), 2, "{errors:?}");
    assert!(
        errors[0].contains("jump into the scope of 'a'"),
        "{errors:?}"
    );
    assert!(
        errors[1].ends_with("jump into statement expression"),
        "{errors:?}"
    );
}

/// Leaving a statement expression is allowed, as are jumps and switches wholly
/// inside one and a computed `goto`, which gcc leaves undiagnosed. These run,
/// so the jumps are shown to land where they should.
#[test]
fn diagnostics_legal_jumps_around_statement_expressions_are_accepted() {
    let src = r#"
int out(int x) { int y = ({ if (x) goto bail; x + 1; }); return y; bail: return -1; }
int within(void) { return ({ int r = 1; goto M; r = 5; M: r; }); }
int nested_out(int x) { return ({ int r = ({ if (x) goto P; 10; }); P: r; }); }
int back(int x) { L: x = ({ if (x > 3) goto L2; x + 1; }); if (x < 3) goto L; L2: return x; }
int sw(int x) { return ({ int r = 0; switch (x) { case 1: r = 7; break; default: r = 9; } r; }); }
int brk(int x) { for (;;) { x = ({ if (x > 5) break; x + 2; }); } return x; }
int main(void) {
    if (out(0) != 1 || out(2) != -1) return 1;
    if (within() != 1) return 2;
    if (nested_out(0) != 10) return 3;
    if (back(0) != 3) return 4;
    if (sw(1) != 7 || sw(4) != 9) return 5;
    if (brk(0) != 6) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("se_legal_jumps", src, &[]), 0);
    // gcc documents a computed `goto` into a statement expression as
    // undefined rather than diagnosing it, so it compiles; running it would
    // prove nothing.
    compile_expect_ok(
        "se_computed_goto",
        "int f(int x) { void *p = &&Q; goto *p; return ({ Q: x; }); }\n",
    );
    // The address of a label that is never evaluated still names a block --
    // one nothing reaches -- rather than a symbol no block defines.
    let src = "int f(int x) { void *p = &&U; return (int)sizeof(({ U: x; })) + (p != 0); }\n\
               int main(void) { return f(1) != 5; }\n";
    assert_eq!(compile_and_run("se_unevaluated_label_address", src, &[]), 0);
}

// ============================================================================
// Array compatibility when one side has no extent (C17 6.7.6.2p6)
// ============================================================================

/// A folded shift agrees with the same shift computed at run time.
///
/// The count is masked to the operand width, as the hardware does. 6.5.7p3
/// makes a count outside `[0, width)` undefined and gcc has no single answer
/// for it either -- it folds `1 << 64` to 0 but leaves `-1 >> 64` at -1 -- so
/// what this pins is c17's *self*-consistency: an enumerator, an array bound
/// and a run-time expression must not disagree. Recorded at #C125, which is
/// the warning gcc has and c17 does not.
#[test]
fn diagnostics_folded_shift_agrees_with_the_runtime_one() {
    assert_eq!(
        compile_and_run(
            "folded_shift_matches_runtime",
            "enum E { A = 1 << 64, B = 1 >> 64, C = (char)1 << 20, D = 1LL << 40 };\n\
             int main(void) {\n\
             volatile int one = 1, sixty_four = 64;\n\
             if (A != (one << sixty_four)) return 1;\n\
             if (B != (one >> sixty_four)) return 2;\n\
             if (C != 1048576) return 3;\n\
             if (D != 1099511627776LL) return 4;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// The largest object that *is* describable keeps working, and `sizeof` agrees
/// with gcc for it — the bound has to be a diagnostic at the edge, not a cap
/// that moved.
#[test]
fn diagnostics_largest_describable_object_is_accepted() {
    for (name, src) in [
        // Past the old 512 MB cap, and well within what C allows.
        (
            "array_past_the_old_cap",
            "char big[2000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_of_int_past_the_old_cap",
            "int big[2000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_two_dimensions",
            "char big[16385][32768];\nint main(void){ return 0; }\n",
        ),
        (
            "struct_sum_past_the_old_cap",
            "struct S { char a[400000000]; char b[400000000]; } s;\nint main(void){ return 0; }\n",
        ),
        (
            "array_near_ptrdiff_max",
            "typedef char T[2000000000000000000L];\nint main(void){ return 0; }\n",
        ),
        (
            "array_at_ptrdiff_max",
            "typedef char T[9223372036854775807L];\nint main(void){ return 0; }\n",
        ),
        (
            "struct_past_u64_bits",
            "struct S { short buf[(1L << 62) - 256]; int a, b, c, d; };\n\
             int main(void){ return sizeof(struct S) != 9223372036854775312UL; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }

    // And the sizes are the ones gcc reports, so that moving the bound cannot
    // quietly move an answer with it.
    //
    // Everything past the old cap is asked of a *type*, not of an object.
    // `sizeof` needs no storage, and defining the objects instead made the
    // program ask its loader for gigabytes of zero-fill: a `char b[2000000000]`
    // here is `.zerofill` of 2 GB in the Mach-O, and macOS refuses to map it
    // ("dyld cache not loaded: syscall to map cache into shared region
    // failed") where Linux's overcommit had hidden the cost. The one object
    // that is defined is the size the old bound allowed, which is what pins
    // that the bound moved without the answers moving.
    assert_eq!(
        compile_and_run(
            "object_sizes_are_exact",
            "char a[536870911];\n\
             struct S { char x[100000000]; char y[100000000]; } s;\n\
             typedef char PastOldCap[2000000000L];\n\
             typedef char Huge[2000000000000000000L];\n\
             typedef struct { char x[4000000000L]; char y[4000000000L]; } BigSum;\n\
             int main(void) {\n\
             if (sizeof a != 536870911UL) return 1;\n\
             if (sizeof s != 200000000UL) return 2;\n\
             if (sizeof (PastOldCap) != 2000000000UL) return 3;\n\
             if (sizeof (Huge) != 2000000000000000000UL) return 4;\n\
             if (sizeof (BigSum) != 8000000000UL) return 5;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// What 6.6 *does* let a floating constant do, and what the two folders had to
/// agree on before they became one.
#[test]
fn diagnostics_shared_constant_folder_answers_alike() {
    assert_eq!(
        compile_and_run(
            "shared_constant_folder",
            "struct S { int x; int y; };\n\
             const int c = 5;\n\
             int arr[10];\n\
             int *p = &arr[c - 3];\n\
             int w = c + 1;\n\
             enum E { A = 3 };\n\
             int main(void) {\n\
             /* a cast of a floating constant, folded in floating point */\n\
             int cast_fold[(int)(1.5 + 1.5)];\n\
             /* a comparison of floating operands is an integer constant */\n\
             int cmp[1.5 > 1.0 ? 4 : 8];\n\
             /* _Alignof, which only one of the two folders used to know */\n\
             int aligned[_Alignof(double)];\n\
             /* the pre-<stddef.h> offsetof idiom */\n\
             int off[(int)(unsigned long)&((struct S *)0)->y];\n\
             _Static_assert(1.5 > 1.0, \"\");\n\
             _Static_assert((unsigned)-1 > 0, \"\");\n\
             if (sizeof cast_fold != 12) return 1;\n\
             if (sizeof cmp != 16) return 2;\n\
             if (sizeof aligned != 32) return 3;\n\
             if (sizeof off != 16) return 4;\n\
             if (w != 6) return 5;\n\
             if (p != &arr[2]) return 6;\n\
             if (A != 3) return 7;\n\
             if (!__builtin_constant_p(3.14)) return 8;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// Every operation is evaluated *in* a type, and its result narrowed to that
/// type before the next one sees it.
///
/// The folder carried a full-width `i128` instead, so `-1u / 3u` was 0 rather
/// than 1431655765 -- the negative was still negative when the division saw
/// it -- `(unsigned)-1 % 7u` was 4294967295 rather than 3, and a cast did not
/// convert at all: `(unsigned char)-1` was -1, `(short)70000` was 70000. Each
/// value below is gcc's. Recorded at #C126.
/// The folder has to read a constant at the operand's width, in the
/// signedness the *opcode* implies. An `i128` in the IR holds whatever bit
/// pattern the front end built -- `(int)0xFFFFFFFFu` emits no instruction at
/// all, so the constant still reads 4294967295 while every consumer now
/// treats it as a signed `int`.
///
/// Division, remainder and the ordering comparisons are the operations that
/// notice: unlike add/sub/mul they are not congruent modulo 2^n, so they read
/// the whole value and its sign. Everything here is correct at `-O0` and was
/// wrong at `-O`, which is what localizes it to the folder.
#[test]
fn diagnostics_signed_constants_fold_at_their_own_width() {
    let code = "int main(void) {\n\
         /* signed division and remainder of a same-width cast constant */\n\
         if ((int)0xFFFFFFFFu / 2 != 0) return 1;\n\
         if ((int)0xFFFFFFFFu % 3 != -1) return 2;\n\
         if ((int)0x80000000u / 2 != -1073741824) return 3;\n\
         if ((int)0x80000000u % 7 != -2) return 4;\n\
         /* the same value, spelled as a negative literal, must agree */\n\
         if ((-1) / 2 != 0) return 5;\n\
         if ((-1) % 3 != -1) return 6;\n\
         /* signed ordering comparisons */\n\
         if (!((int)0xFFFFFFFFu < 0)) return 7;\n\
         if (!((int)0xFFFFFFFFu <= -1)) return 8;\n\
         if (!((int)0xFFFFFFFFu == -1)) return 9;\n\
         if ((int)0xFFFFFFFFu > 0) return 10;\n\
         if ((int)0xFFFFFFFFu >= 0) return 11;\n\
         /* unsigned operators on the same bits must stay unsigned */\n\
         if (0xFFFFFFFFu / 2u != 2147483647u) return 12;\n\
         if (0xFFFFFFFFu % 3u != 0u) return 13;\n\
         if (0xFFFFFFFFu < 1u) return 14;\n\
         if (!(0xFFFFFFFFu > 1u)) return 15;\n\
         /* 64-bit, where the narrowing is not to 32 */\n\
         if ((long long)0xFFFFFFFFFFFFFFFFull / 2 != 0) return 16;\n\
         if ((long long)0xFFFFFFFFFFFFFFFFull < 0 ? 0 : 1) return 17;\n\
         /* narrower operands promote to int before dividing */\n\
         if ((signed char)-1 / 2 != 0) return 18;\n\
         if ((short)-1 % 3 != -1) return 19;\n\
         return 0;\n\
         }\n";
    assert_eq!(
        compile_and_run("signed_constants_fold_at_their_own_width", code, &[]),
        0
    );
}

#[test]
fn diagnostics_constants_fold_at_their_own_width() {
    assert_eq!(
        compile_and_run(
            "constants_fold_at_their_own_width",
            "typedef unsigned __int128 u128;\n\
             int main(void) {\n\
             /* division and remainder see an unsigned operand as unsigned */\n\
             if (-1u / 3u != 1431655765u) return 1;\n\
             if ((unsigned)-1 % 7u != 3u) return 2;\n\
             if ((0u - 1u) / 2u != 2147483647u) return 3;\n\
             if (-1ull / 3ull != 6148914691236517205ull) return 4;\n\
             /* a right shift of an unsigned value is logical */\n\
             if ((-1u >> 1) != 2147483647u) return 5;\n\
             if ((-1ull >> 1) != 9223372036854775807ull) return 6;\n\
             if ((~0u >> 28) != 15u) return 7;\n\
             /* 128 bits, where narrowing cannot help and signedness must */\n\
             u128 thirds = ((u128)6148914691236517205ull << 64) | 6148914691236517205ull;\n\
             if ((u128)-1 / 3 != thirds) return 8;\n\
             if ((long long)((u128)-1 >> 1) != -1) return 9;\n\
             /* a cast converts */\n\
             if ((int)(unsigned char)-1 != 255) return 10;\n\
             if ((int)(unsigned char)300 != 44) return 11;\n\
             if ((int)(short)70000 != 4464) return 12;\n\
             if ((int)(signed char)200 != -56) return 13;\n\
             if ((int)(unsigned short)-1 != 65535) return 14;\n\
             /* ... and a conversion to _Bool gives 0 or 1, not the low byte */\n\
             if ((int)(_Bool)2 != 1) return 15;\n\
             if ((int)(_Bool)256 != 1) return 16;\n\
             /* integer promotion still widens: these are int arithmetic */\n\
             if ((char)100 + (char)100 != 200) return 17;\n\
             if ((short)30000 + (short)30000 != 60000) return 18;\n\
             if ((unsigned char)200 + (unsigned char)200 != 400) return 19;\n\
             /* and signed arithmetic wraps at its own width */\n\
             if (4294967295u + 1u != 0u) return 20;\n\
             if ((unsigned)(1u << 31) != 2147483648u) return 21;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );

    // The same expressions as *constant* contexts, where only the folder can
    // answer: a run-time path that happened to be right would hide the bug.
    assert_eq!(
        compile_and_run(
            "constant_contexts_fold_alike",
            "enum E {\n\
             A = -1u / 3u,\n\
             B = (unsigned)-1 % 7u,\n\
             C = (int)(unsigned char)300,\n\
             D = (int)(short)70000,\n\
             F = (int)(_Bool)2,\n\
             G = (int)(-1u >> 1)\n\
             };\n\
             static unsigned s_div = -1u / 3u;\n\
             static int s_cast = (int)(unsigned char)300;\n\
             int main(void) {\n\
             int a[(int)(unsigned char)300];\n\
             if (A != 1431655765) return 1;\n\
             if (B != 3) return 2;\n\
             if (C != 44) return 3;\n\
             if (D != 4464) return 4;\n\
             if (F != 1) return 5;\n\
             if (G != 2147483647) return 6;\n\
             if (s_div != 1431655765u) return 7;\n\
             if (s_cast != 44) return 8;\n\
             if (sizeof a != 44 * sizeof(int)) return 9;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

/// 6.4.4.1p5 picks the first type that can represent the value, for a
/// `u`-suffixed constant as much as an unsuffixed one.
///
/// Every `u` suffix took `unsigned int` regardless of magnitude, so
/// `0xaaaaaaaaaaaaaaabu` had a four-byte type. That was survivable while
/// constants were carried at full width and only `sizeof` was wrong; once they
/// folded at their own width it truncated the value, and CPython's
/// `math.comb(5, 2)` returned 85899345930. Recorded at #C127.
#[test]
fn diagnostics_unsigned_suffix_widens_by_magnitude() {
    assert_eq!(
        compile_and_run(
            "unsigned_suffix_widens_by_magnitude",
            "int main(void) {\n\
             if (sizeof 1u != 4) return 1;\n\
             if (sizeof 0xFFFFFFFFu != 4) return 2;\n\
             if (sizeof 0x100000000u != 8) return 3;\n\
             if (sizeof 4294967296u != 8) return 4;\n\
             if (sizeof 0xaaaaaaaaaaaaaaabu != 8) return 5;\n\
             if (sizeof 18446744073709551615u != 8) return 6;\n\
             if (0xaaaaaaaaaaaaaaabu != 12297829382473034411ULL) return 7;\n\
             if (0x100000000u != 4294967296ULL) return 8;\n\
             /* the value that made math.comb wrong: a 64-bit product whose\n\
                left operand had been truncated to 32 bits */\n\
             if (0xfu * 0xaaaaaaaaaaaaaaabu != 5) return 9;\n\
             /* unsuffixed and l-suffixed spellings were already right */\n\
             if (sizeof 0xaaaaaaaaaaaaaaab != 8) return 10;\n\
             if (sizeof 1ul != 8) return 11;\n\
             if (sizeof 1ull != 8) return 12;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );

    // Through a static table, which is how CPython hit it: the initializer is
    // emitted from the folded value, so a truncated literal reaches .rodata.
    assert_eq!(
        compile_and_run(
            "wide_unsigned_literal_in_a_static_table",
            "static const unsigned long t[] = { 0xfu, 0xaaaaaaaaaaaaaaabu };\n\
             int main(void) {\n\
             unsigned long a = t[0], b = t[1];\n\
             if (b != 12297829382473034411UL) return 1;\n\
             if (a * b != 5) return 2;\n\
             return 0;\n\
             }\n",
            &[],
        ),
        0
    );
}

// ============================================================================
// #C116 — the lock-free atomic ceiling
// ============================================================================

/// The other side of the same rule: an aggregate *at* a lock-free width must
/// draw no diagnostic at all, because it is now genuinely atomic (#C116).
///
/// Without this, the test above passes just as well against a compiler that
/// warns on every `_Atomic` aggregate -- which is what c17 used to do.
#[test]
fn diagnostics_lock_free_atomic_aggregate_is_silent() {
    let src = r#"
struct S1 { char a; };
struct S2 { short a; };
struct S4 { int a; };
struct S8 { int a, b; };
union  U4 { int i; float f; };
_Atomic struct S1 g1;
_Atomic struct S2 g2;
_Atomic struct S4 g4;
_Atomic struct S8 g8;
_Atomic union  U4 gu;
void f(struct S1 a, struct S2 b, struct S4 c, struct S8 d, union U4 e) {
    g1 = a; g2 = b; g4 = c; g8 = d; gu = e;
}
"#;
    let c = create_c_file("atomic_lock_free_aggregate_silent", src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
    assert!(run.success, "should compile: {}", run.stderr);
    assert!(
        !run.stderr.contains("is not atomic"),
        "an aggregate at a lock-free width must not warn, got:\n{}",
        run.stderr
    );
}

/// The discarded diagnostic is only half the defect: the fallback's two
/// *spurious* messages were the visible half, and they must be gone.
#[test]
fn diagnostics_abstract_declarator_does_not_cascade() {
    let c = create_c_file(
        "abstract_no_cascade",
        "int main(void){ return sizeof(char[-1]); }\n",
    );
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);

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
        let c = create_c_file(name, src);
        let path = c.path().to_string_lossy().to_string();
        let run = run_c17(&[flag, "-S", "-o", "/dev/null", &path]);
        assert!(run.success, "{} should still compile: {}", name, run.stderr);
        assert!(
            !run.stderr.contains("shift count"),
            "{} did not silence the warning:\n{}",
            flag,
            run.stderr
        );
    }
}

/// C17 6.5.7p3: "the type of the result is that of the promoted left operand".
/// The right operand's type never reaches the result, and c17 took the usual
/// arithmetic conversions instead -- so `1 << 1L` came out `long` and
/// `sizeof(1 << 1L)` answered 8 where gcc answers 4. That width is also what
/// the warning above measures against, so the two had to be fixed together.
#[test]
fn diagnostics_shift_result_type_is_the_promoted_left_operand() {
    // Run it: compiling proves nothing here, since the wrong type compiles
    // just as cleanly as the right one.
    assert_eq!(
        compile_and_run(
            "shift_result_type",
            r#"
int main(void) {
    if (sizeof(1 << 1L) != sizeof(int)) return 1;
    if (sizeof(1L << 1) != sizeof(long)) return 2;
    if (sizeof((char)1 << 1) != sizeof(int)) return 3;
    if (sizeof(1U << 1L) != sizeof(unsigned int)) return 4;
    return 0;
}
"#,
            &[]
        ),
        0
    );
}

/// #C132: a diagnostic must name the type the source could have written.
///
/// `int m[4][8]; int *p = m;` reported the pointee as `int[8] *`, which reads
/// as "array of pointers" -- the other type entirely. `format_type` built the
/// spelling left to right and had no notion of a declarator's inside-out
/// reading, so it could not parenthesize. gcc says `int (*)[8]`.
#[test]
fn diagnostics_pointer_to_array_is_spelled_as_a_declarator() {
    let c = create_c_file("spell_ptr_to_array", "int m[4][8];\nint *p = m;\n");
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);

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
    let c = create_c_file(
        "spell_fn_ptr",
        "int f(void);\nint (*fp)(void) = f;\nint bad = fp;\n",
    );
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);

    assert!(
        run.stderr.contains("int (*)(void)"),
        "expected `int (*)(void)`, got:\n{}",
        run.stderr
    );
}

// ============================================================================
// -fpermissive — two pre-C99 constructs, error by default
// ============================================================================

/// Compile `content` with the given extra flags and hand back the run.
fn compile_with(name: &str, content: &str, flags: &[&str]) -> crate::common::C17Run {
    let c_file = create_c_file(name, content);
    let path = c_file.path().to_string_lossy().to_string();
    let mut args: Vec<&str> = vec!["-S", "-o", "/dev/null"];
    args.extend_from_slice(flags);
    args.push(&path);
    run_c17(&args)
}

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

    let strict = compile_with("permissive_off_int", src, &[]);
    assert!(!strict.success, "implicit int must be an error by default");
    assert!(
        strict.stderr.contains("error:") && strict.stderr.contains("type specifier missing"),
        "default build lost the implicit-int error:\n{}",
        strict.stderr
    );

    let lax = compile_with("permissive_on_int", src, &["-fpermissive"]);
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

    let strict = compile_with("permissive_off_fn", src, &[]);
    assert!(
        !strict.success && strict.stderr.contains("undeclared identifier"),
        "a call to an undeclared function must be an error by default:\n{}",
        strict.stderr
    );

    let lax = compile_with("permissive_on_fn", src, &["-fpermissive"]);
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
        let run = compile_with(name, src, &["-fpermissive"]);
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
        let run = compile_with(name, src, &["-fpermissive"]);
        assert!(
            !run.success && run.stderr.contains(expected),
            "{name}: -fpermissive should not have relaxed this:\n{}",
            run.stderr
        );
    }
}

/// The converse: an ordinary C99 inline definition is **not** an error, even
/// though it too has no out-of-line copy here. Its external definition may be
/// in another translation unit, which is exactly the idiom a header uses, so
/// an unsubstituted call is correct and the linker resolves it.
#[test]
fn diagnostics_plain_inline_definition_is_not_an_error() {
    let code = r#"
inline int helper(int a) { return a + 1; }
int use(int a) { return helper(a); }
int main(void) { return use(1) == 2 ? 0 : 1; }
"#;
    assert_eq!(compile_and_run("diag_plain_inline_ok", code, &[]), 0);
}

/// An `always_inline` call the inliner merely *declined* is not an error
/// either.
///
/// The caps on caller size and on recursive stack depth are c17's own -- gcc
/// has no counterpart -- so a refusal by one of them says nothing about
/// whether an out-of-line definition exists. Reporting it rejected programs
/// gcc compiles, and this is the shape that reaches it: glibc's
/// `__fortify_function` is exactly an `extern __inline` `gnu_inline`
/// `always_inline` definition, and any recursive function over a few hundred
/// instructions calling one declines the splice.
///
/// Two translation units, because that is the arrangement the idiom names: the
/// header's inline definition promises an out-of-line copy elsewhere, and the
/// call the inliner left standing is resolved against it.
#[test]
fn diagnostics_always_inline_declined_for_stack_depth_is_not_an_error() {
    let header_user = r#"
extern __inline __attribute__((__gnu_inline__, __always_inline__))
int helper(int x) { return x + 1; }

/* Large enough that the recursive-caller stack guard turns the splice down. */
int rec(int i)
{
    int t = 0;
    if (i <= 0) return 0;
    t += helper(i + 0);
    t += helper(i + 1);
    t += helper(i + 2);
    t += helper(i + 3);
    t += helper(i + 4);
    t += helper(i + 5);
    t += helper(i + 6);
    t += helper(i + 7);
    t += helper(i + 8);
    t += helper(i + 9);
    t += helper(i + 10);
    t += helper(i + 11);
    t += helper(i + 12);
    t += helper(i + 13);
    t += helper(i + 14);
    t += helper(i + 15);
    t += helper(i + 16);
    t += helper(i + 17);
    t += helper(i + 18);
    t += helper(i + 19);
    t += helper(i + 20);
    t += helper(i + 21);
    t += helper(i + 22);
    t += helper(i + 23);
    t += helper(i + 24);
    t += helper(i + 25);
    t += helper(i + 26);
    t += helper(i + 27);
    t += helper(i + 28);
    t += helper(i + 29);
    t += helper(i + 30);
    t += helper(i + 31);
    t += helper(i + 32);
    t += helper(i + 33);
    t += helper(i + 34);
    t += helper(i + 35);
    t += helper(i + 36);
    t += helper(i + 37);
    t += helper(i + 38);
    t += helper(i + 39);
    t += helper(i + 40);
    t += helper(i + 41);
    t += helper(i + 42);
    t += helper(i + 43);
    t += helper(i + 44);
    t += helper(i + 45);
    t += helper(i + 46);
    t += helper(i + 47);
    t += helper(i + 48);
    t += helper(i + 49);
    t += helper(i + 50);
    t += helper(i + 51);
    t += helper(i + 52);
    t += helper(i + 53);
    t += helper(i + 54);
    t += helper(i + 55);
    t += helper(i + 56);
    t += helper(i + 57);
    t += helper(i + 58);
    t += helper(i + 59);
    t += helper(i + 60);
    t += helper(i + 61);
    t += helper(i + 62);
    t += helper(i + 63);
    t += helper(i + 64);
    t += helper(i + 65);
    t += helper(i + 66);
    t += helper(i + 67);
    t += helper(i + 68);
    t += helper(i + 69);
    t += helper(i + 70);
    t += helper(i + 71);
    t += helper(i + 72);
    t += helper(i + 73);
    t += helper(i + 74);
    t += helper(i + 75);
    t += helper(i + 76);
    t += helper(i + 77);
    t += helper(i + 78);
    t += helper(i + 79);
    t += helper(i + 80);
    t += helper(i + 81);
    t += helper(i + 82);
    t += helper(i + 83);
    t += helper(i + 84);
    t += helper(i + 85);
    t += helper(i + 86);
    t += helper(i + 87);
    t += helper(i + 88);
    t += helper(i + 89);
    t += helper(i + 90);
    t += helper(i + 91);
    t += helper(i + 92);
    t += helper(i + 93);
    t += helper(i + 94);
    t += helper(i + 95);
    t += helper(i + 96);
    t += helper(i + 97);
    t += helper(i + 98);
    t += helper(i + 99);
    t += helper(i + 100);
    t += helper(i + 101);
    t += helper(i + 102);
    t += helper(i + 103);
    t += helper(i + 104);
    t += helper(i + 105);
    t += helper(i + 106);
    t += helper(i + 107);
    t += helper(i + 108);
    t += helper(i + 109);
    t += helper(i + 110);
    t += helper(i + 111);
    t += helper(i + 112);
    t += helper(i + 113);
    t += helper(i + 114);
    t += helper(i + 115);
    t += helper(i + 116);
    t += helper(i + 117);
    t += helper(i + 118);
    t += helper(i + 119);
    t += helper(i + 120);
    t += helper(i + 121);
    t += helper(i + 122);
    t += helper(i + 123);
    t += helper(i + 124);
    t += helper(i + 125);
    t += helper(i + 126);
    t += helper(i + 127);
    t += helper(i + 128);
    t += helper(i + 129);
    t += helper(i + 130);
    t += helper(i + 131);
    t += helper(i + 132);
    t += helper(i + 133);
    t += helper(i + 134);
    t += helper(i + 135);
    t += helper(i + 136);
    t += helper(i + 137);
    t += helper(i + 138);
    t += helper(i + 139);
    t += helper(i + 140);
    t += helper(i + 141);
    t += helper(i + 142);
    t += helper(i + 143);
    t += helper(i + 144);
    t += helper(i + 145);
    t += helper(i + 146);
    t += helper(i + 147);
    t += helper(i + 148);
    t += helper(i + 149);
    t += helper(i + 150);
    t += helper(i + 151);
    t += helper(i + 152);
    t += helper(i + 153);
    t += helper(i + 154);
    t += helper(i + 155);
    t += helper(i + 156);
    t += helper(i + 157);
    t += helper(i + 158);
    t += helper(i + 159);
    t += helper(i + 160);
    t += helper(i + 161);
    t += helper(i + 162);
    t += helper(i + 163);
    t += helper(i + 164);
    t += helper(i + 165);
    t += helper(i + 166);
    t += helper(i + 167);
    t += helper(i + 168);
    t += helper(i + 169);
    t += helper(i + 170);
    t += helper(i + 171);
    t += helper(i + 172);
    t += helper(i + 173);
    t += helper(i + 174);
    t += helper(i + 175);
    t += helper(i + 176);
    t += helper(i + 177);
    t += helper(i + 178);
    t += helper(i + 179);
    t += helper(i + 180);
    t += helper(i + 181);
    t += helper(i + 182);
    t += helper(i + 183);
    t += helper(i + 184);
    t += helper(i + 185);
    t += helper(i + 186);
    t += helper(i + 187);
    t += helper(i + 188);
    t += helper(i + 189);
    t += helper(i + 190);
    t += helper(i + 191);
    t += helper(i + 192);
    t += helper(i + 193);
    t += helper(i + 194);
    t += helper(i + 195);
    t += helper(i + 196);
    t += helper(i + 197);
    t += helper(i + 198);
    t += helper(i + 199);
    return t + rec(i - 1);
}

int main(void) { return rec(1) == 0 ? 1 : 0; }
"#;
    let out_of_line = r#"
int helper(int x) { return x + 1; }
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run_two_units(
                &format!("diag_always_inline_declined{opt}"),
                header_user,
                out_of_line,
                &[opt.to_string()],
            ),
            0,
            "at {opt}"
        );
    }
}

// ============================================================================
// What `-fpermissive` relaxes
// ============================================================================

/// A `vector_size` value passed to or returned from a function is refused
/// only where the target's convention has no type that travels as gcc
/// passes it: a one-lane `float` vector on aarch64. On x86-64 it goes in
/// memory, as gcc's does, and every other vector goes as its carrier.
#[test]
fn diagnostics_vector_passing_is_refused_only_without_a_carrier() {
    let prelude = "typedef float V1SF __attribute__((vector_size(4)));\n\
                   typedef int V2SI __attribute__((vector_size(8)));\n\
                   long f(); long l; int c;\n";
    let compile = |name: &str, body: &str, target: &str| {
        let c = create_c_file(name, &format!("{prelude}{body}\n"));
        let path = c.path().to_string_lossy().into_owned();
        run_c17(&["--target", target, "-S", "-o", "/dev/null", &path])
    };
    for (name, body) in [
        ("argument", "void t(void) { V1SF v = {1}; f(v); }"),
        ("parameter", "long t(V1SF v) { return 0; }"),
        ("return", "V1SF t(void) { V1SF v = {1}; return v; }"),
    ] {
        let a64 = compile(
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
        let x86 = compile(
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

/// What the storage model gets right is still accepted: declaring a vector,
/// `sizeof`, `&v`, `v[i]`, a vector member, an initializer, and copying a
/// struct that holds one -- what glibc's `<link.h>` needs.
#[test]
fn diagnostics_vector_storage_is_accepted() {
    let src = r#"
typedef int V2SI __attribute__((vector_size(8)));
typedef float V4SF __attribute__((vector_size(16), aligned(16)));
struct regs { V4SF x[4]; long l; };
struct regs g;
int main(void)
{
    V2SI v = { 1, 2 };
    V2SI *p = &v;
    struct regs r = { 0 };
    r.x[1][2] = 3.0f;
    if (sizeof v != 8 || sizeof(struct regs) != 80) return 1;
    if (v[0] + (*p)[1] != 3) return 2;
    g = r;
    return g.x[1][2] == 3.0f ? 0 : 3;
}
"#;
    assert_eq!(compile_and_run("vector_storage", src, &[]), 0);
}

/// Naming a vector where its value is discarded is not a value use. `(void)v`
/// is how an unused variable is marked used, and it was refused: the cast
/// check did not tell a cast to `void` from a conversion. An expression
/// statement, the left operand of a comma, and the operands of `sizeof`,
/// `__alignof__` and `__typeof__` read nothing either.
#[test]
fn diagnostics_vector_discarded_value_is_accepted() {
    let src = r#"
typedef int V __attribute__((vector_size(8)));
int main(void)
{
    V v, w;
    (void)v;
    v;
    (v, 1);
    (void)sizeof v;
    (void)__alignof__(v);
    __typeof__(v) u;
    (void)u;
    v[0] = 3;
    w[1] = v[0];
    return w[1] == 3 ? 0 : 1;
}
"#;
    assert_eq!(compile_and_run("vector_discarded", src, &[]), 0);
}

/// `__builtin_signbit` takes any real floating type, as gcc's does, and
/// refuses anything else as gcc does. A `long double` used to reach the
/// `double` emitter unconverted.
#[test]
fn diagnostics_signbit_is_type_generic() {
    let src = r#"
int main(void)
{
    volatile long double neg = -1.0L, pos = 1.0L, nz = -0.0L;
    volatile float f = -2.0f;
    volatile double d = -0.0;
    if (!__builtin_signbit(neg) || __builtin_signbit(pos) || !__builtin_signbit(nz)) return 1;
    if (!__builtin_signbit(f) || !__builtin_signbit(d)) return 2;
    return 0;
}
"#;
    assert_eq!(compile_and_run("signbit_generic", src, &[]), 0);
    let opts = vec!["-O2".to_string()];
    assert_eq!(compile_and_run("signbit_generic_o2", src, &opts), 0);
    compile_expect_error(
        "signbit_int",
        "int t(int x) { return __builtin_signbit(x); }\n",
        "non-floating-point argument",
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
            let c = create_c_file(name, src);
            let out = c.path().with_extension("s");
            let run = run_c17(&[
                "--target",
                target,
                "-S",
                "-o",
                &out.to_string_lossy(),
                &c.path().to_string_lossy(),
            ]);
            let _ = std::fs::remove_file(&out);
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

/// With several operands, an error inside a header names the translation unit
/// that included it. The note took its file name from the first stream ever
/// opened, so the second operand's error was reported against the first
/// operand: `one.c: note: in included file (through two.c)`.
#[test]
fn diagnostics_include_note_names_its_own_translation_unit() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_include_note_")
        .tempdir()
        .unwrap();
    let one = dir.path().join("one.c");
    let two = dir.path().join("two.c");
    std::fs::write(&one, "int main(void){return 0;}\n").unwrap();
    std::fs::write(&two, "#include \"bad.h\"\n").unwrap();
    std::fs::write(dir.path().join("bad.h"), "int x = undeclared_thing;\n").unwrap();
    let exe = dir.path().join("t.out");

    let r = run_c17(&[
        &one.to_string_lossy(),
        &two.to_string_lossy(),
        "-o",
        &exe.to_string_lossy(),
    ]);
    assert!(!r.success, "the header's error must fail the build");
    let note = r
        .stderr
        .lines()
        .find(|l| l.contains("in included file"))
        .unwrap_or_else(|| panic!("expected an include note:\n{}", r.stderr));
    assert!(
        note.starts_with(&*two.to_string_lossy()),
        "the note must name two.c, which included the header:\n{}",
        r.stderr
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
        let c = create_c_file("alias_darwin", src);
        let out = c.path().with_extension("s");
        let run = run_c17(&[
            "--target",
            target,
            "-S",
            "-o",
            &out.to_string_lossy(),
            &c.path().to_string_lossy(),
        ]);
        let _ = std::fs::remove_file(&out);
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
    let c = create_c_file("type_name_vla_member", src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&["-S", "-o", "/dev/null", &path]);
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

// ============================================================================
// One declaration-specifier loop (C17 6.7.2, 6.7.7)
// ============================================================================

/// Compile `src` and return its stderr, requiring that it was rejected.
fn rejected_stderr(name: &str, src: &str, extra: &[&str]) -> String {
    let c = create_c_file(name, src);
    let path = c.path().to_string_lossy().to_string();
    let mut args = extra.to_vec();
    args.extend(["-S", "-o", "/dev/null", &path]);
    let run = run_c17(&args);
    assert!(!run.success, "'{name}' should have been rejected:\n{src}");
    run.stderr
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

/// `__float128` names a type only where the target has one. A declaration
/// said so; a type-name broke out of the loop and re-read the keyword as an
/// expression, reporting an undeclared identifier.
#[test]
fn diagnostics_float128_type_name_on_a_target_without_it() {
    let stderr = rejected_stderr(
        "tn_float128",
        "int f(void){ return sizeof(__float128); }\n",
        &["--target", "aarch64-apple-darwin"],
    );
    assert!(
        stderr.contains("__float128 is not supported on this target"),
        "{stderr}"
    );
    assert!(!stderr.contains("undeclared identifier"), "{stderr}");
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

// ============================================================================
// One path binds a declarator, whichever position it holds
// ============================================================================

// ==== numeric escapes out of range (C17 6.4.4.4p9) ====

/// An octal or hex escape's value must be representable in the literal's
/// element type: `unsigned char` for a plain literal, and the unsigned type
/// of `wchar_t`, `char16_t` or `char32_t` for a prefixed one. gcc warns and
/// truncates; c17 was silent. A constraint gcc only warns about is an error
/// here, and `-fpermissive` makes it a warning with gcc's truncation.
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
        let strict = compile_with(name, src, &[]);
        assert!(
            !strict.success && strict.stderr.contains("error:") && strict.stderr.contains(msg),
            "{name}: expected an error mentioning {msg:?}:\n{}",
            strict.stderr
        );
        let lax = compile_with(name, src, &["-fpermissive"]);
        assert!(
            lax.success && lax.stderr.contains("warning:") && lax.stderr.contains(msg),
            "{name}: -fpermissive should warn {msg:?}:\n{}",
            lax.stderr
        );
    }
}

/// Under `-fpermissive` the program keeps gcc's truncation to the low bits.
#[test]
fn diagnostics_escape_out_of_range_truncates_under_fpermissive() {
    let src = r#"
typedef __CHAR16_TYPE__ char16_t;
int main(void) {
    const char16_t *u = u"\x12345";
    if ((unsigned char)"\x141"[0] != 0x41) return 1;
    if ((unsigned char)'\777' != 0xff) return 2;
    if (u[0] != 0x2345) return 3;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("esc_truncate", src, &["-fpermissive".to_string()]),
        0
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
        let out = compile_with(name, src, &[]);
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
