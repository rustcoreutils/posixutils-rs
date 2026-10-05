use crate::test_compile::{compile, compile_expect_error, compile_expect_ok};

// ============================================================================
// Jumping into the scope of a variably modified type (C17 6.8.6.1p1)
// ============================================================================

/// Entering the scope of a variably modified identifier without executing its
/// declaration leaves the object's size never computed -- the array is
/// whatever the stack held -- so 6.8.6.1p1 forbids the jump outright. c17
/// accepted every form of it.
///
/// The diagnostic names the declaration that would have been skipped, where
/// gcc names only the kind of type; `Stmt::Goto` carries no position, and the
/// declaration is the more useful thing to point at anyway.
#[test]
fn diagnostics_jump_into_variably_modified_scope_is_rejected() {
    compile_expect_error(
        "goto_into_vla",
        "int main(void){int n=4; goto L; { int a[n]; L: return a[0]; } }\n",
        "jump into the scope of 'a'",
    );
    // The scope runs to the end of the block, so a label after the
    // declaration is inside it even without braces of its own.
    compile_expect_error(
        "goto_past_vla",
        "int main(void){int n=4; goto L; int a[n]; L: return a[0]; }\n",
        "variably modified type",
    );
    // A pointer to a variably modified array is variably modified too.
    compile_expect_error(
        "goto_into_ptr_to_vla",
        "int main(void){int n=4; goto L; { int (*p)[n]; L: return p != 0; } }\n",
        "jump into the scope of 'p'",
    );
    // A declaration in a for-init scopes over the body.
    compile_expect_error(
        "goto_into_for_init_vla",
        "int main(void){int n=4; goto L; for(int a[n];;){ L: return 0; } }\n",
        "variably modified type",
    );
    // Reaching a `case` transfers control from the `switch`, so the same rule
    // applies -- and says so.
    compile_expect_error(
        "switch_into_vla",
        "int main(void){int n=4,k=1; switch(k){ int a[n]; case 1: return a[0]; } return 0; }\n",
        "switch jump into the scope of 'a'",
    );
}

/// The jumps that stay legal. Without these the check could pass by rejecting
/// every `goto` near a VLA, which is the failure mode the whole diagnostics
/// suite exists to prevent.
#[test]
fn diagnostics_legal_jumps_around_variably_modified_scopes_are_accepted() {
    // Out of the scope, not into it.
    compile_expect_ok(
        "goto_out_of_vla",
        "int main(void){int n=4; L: ; { int a[n]; if(a[0]) goto L; } return 0; }\n",
    );
    // Within one scope.
    compile_expect_ok(
        "goto_within_vla",
        "int main(void){int n=4; { int a[n]; L: a[0]=1; if(a[0]) goto L; } return 0; }\n",
    );
    // To a label that precedes the declaration.
    compile_expect_ok(
        "goto_before_vla",
        "int main(void){int n=4; goto L; L: ; { int a[n]; return a[0]; } }\n",
    );
    // An ordinary array is not variably modified.
    compile_expect_ok(
        "goto_into_plain_array",
        "int main(void){goto L; { int a[4]; L: return a[0]; } }\n",
    );
    // The VLA is inside the case, not around it.
    compile_expect_ok(
        "switch_case_holds_vla",
        "int main(void){int n=4,k=1; switch(k){ case 1: { int a[n]; return a[0]; } } return 0; }\n",
    );
}

// ============================================================================
// Jumping into a GNU statement expression
// ============================================================================

/// The `error:` lines c17 prints for `src`, which must fail to compile.
fn compile_errors(name: &str, src: &str) -> Vec<String> {
    let run = compile(name, src, &[]);
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

// ============================================================================
// C17 7.12.14 — the comparison macros take real floating arguments
// ============================================================================

/// C17 7.12.14p1 requires real floating arguments. gcc relaxes that to "both
/// real, at least one floating", and rejects the rest -- two integers, a
/// pointer, a complex value, a structure -- with this message. c17 accepted
/// two integers, citing gcc wrongly, and anything else was converted blindly.
#[test]
fn diagnostics_fp_compare_needs_a_floating_argument() {
    let builtins = [
        "__builtin_isgreater",
        "__builtin_isgreaterequal",
        "__builtin_isless",
        "__builtin_islessequal",
        "__builtin_islessgreater",
        "__builtin_isunordered",
        "__builtin_iseqsig",
    ];
    let bad = [
        ("int", "int"),
        ("char", "long"),
        ("_Bool", "_Bool"),
        ("enum E", "enum E"),
        ("int *", "double"),
        ("double", "void *"),
        ("_Complex double", "double"),
        ("_Complex int", "double"),
        ("struct S", "double"),
    ];
    for (i, b) in builtins.iter().enumerate() {
        for (j, (l, r)) in bad.iter().enumerate() {
            compile_expect_error(
                &format!("fpcmp_bad_{i}_{j}"),
                &format!("enum E {{ X }}; struct S {{ int a; }};\nint f({l} a, {r} b) {{ return {b}(a, b); }}\n"),
                &format!("non-floating-point arguments in call to function '{b}'"),
            );
        }
    }
}

#[test]
fn diagnostics_fp_compare_mixed_real_arguments_are_accepted() {
    let ok = [
        ("double", "double"),
        ("float", "long double"),
        ("int", "double"),
        ("double", "int"),
        ("float", "long"),
        ("_Bool", "double"),
        ("enum E", "float"),
        ("__int128", "double"),
    ];
    for (j, (l, r)) in ok.iter().enumerate() {
        compile_expect_ok(
            &format!("fpcmp_ok_{j}"),
            &format!(
                "enum E {{ X }};\nint f({l} a, {r} b) {{ return __builtin_isgreater(a, b) + __builtin_isunordered(b, a); }}\n"
            ),
        );
    }
    // Through <math.h>, whose macros expand to the builtins.
    compile_expect_ok(
        "fpcmp_math_h",
        "#include <math.h>\nint f(double d, float g, int i) { return isgreater(d, g) + isless(i, d) + isunordered(g, 1); }\n",
    );
}

// ============================================================================
// #C53 — a trailing comma in a parameter list (C17 6.7.6.3)
// ============================================================================

/// A parameter-type-list is a comma-separated list of parameter declarations,
/// optionally followed by `, ...`; nothing else may follow a comma. c17 let
/// the specifier parser supply an implicit `int` for the empty slot, so
/// `void g(int, );` silently declared `void(int, int)` -- and once call arity
/// was checked, the *correct* call `g(1)` became the one rejected. C23 allows
/// the trailing comma; this compiler is C17, and so is gcc here.
#[test]
fn diagnostics_trailing_comma_in_parameter_list_is_rejected() {
    for (name, src) in [
        ("tc_proto", "void g(int, );\nint main(void){return 0;}\n"),
        (
            "tc_defn",
            "void g(int a, ){(void)a;}\nint main(void){return 0;}\n",
        ),
        (
            "tc_two",
            "void g(int, char, );\nint main(void){return 0;}\n",
        ),
        (
            "tc_fnptr",
            "void g(int (*f)(int, ));\nint main(void){return 0;}\n",
        ),
    ] {
        compile_expect_error(name, src, "after ','");
    }
}

/// The list forms that must keep working -- including the call the old
/// behaviour turned into an error.
#[test]
fn diagnostics_ordinary_parameter_lists_are_accepted() {
    compile_expect_ok(
        "pl_correct_call",
        "void g(int);\nvoid g(int a){(void)a;}\nint main(void){ g(1); return 0; }\n",
    );
    compile_expect_ok(
        "pl_variadic",
        "int f(int, ...);\nint main(void){return 0;}\n",
    );
    compile_expect_ok("pl_void", "int f(void);\nint main(void){return 0;}\n");
    compile_expect_ok("pl_two", "int f(int, char);\nint main(void){return 0;}\n");
    compile_expect_ok("pl_empty", "int f();\nint main(void){return 0;}\n");
    compile_expect_ok(
        "pl_knr",
        "int f(a, b) int a, b; { return a+b; }\nint main(void){ return f(1,2)-3; }\n",
    );
}

// ============================================================================
// Universal character names naming forbidden characters (C17 6.4.3p2)
// ============================================================================

/// A UCN "shall not specify a character whose short identifier is less than
/// 00A0 other than 0024 ($), 0040 (@), or 0060 (`), nor one in the range D800
/// through DFFF inclusive."
///
/// Every one was accepted. The surrogate half was the worse of the two: a
/// surrogate has no `char`, so `char::from_u32` failed and both decoders took
/// that for "not an escape" and carried on with the letter `u`.
#[test]
fn diagnostics_forbidden_universal_character_names_are_rejected() {
    for (name, src) in [
        // In an identifier, at the start and in the middle: the lexer has a
        // separate decoder for each.
        (
            "ucn_ident_start",
            "int \\u0061bc = 3;\nint main(void){return 0;}\n",
        ),
        (
            "ucn_ident_mid",
            "int a\\u0062c = 3;\nint main(void){return 0;}\n",
        ),
        // In a string and in a character constant.
        (
            "ucn_string",
            "int main(void){ const char *s = \"\\u0041\"; return s[0]-65; }\n",
        ),
        (
            "ucn_charconst",
            "int main(void){ return (int)(char)'\\u0041' - 65; }\n",
        ),
        // A control character, and both ends of the surrogate range.
        (
            "ucn_space",
            "int main(void){ const char *s = \"\\u0020\"; return s[0]-32; }\n",
        ),
        (
            "ucn_surrogate_lo",
            "int main(void){ const char *s = \"\\ud800\"; return s[0]; }\n",
        ),
        (
            "ucn_surrogate_hi",
            "int main(void){ const char *s = \"\\udfff\"; return s[0]; }\n",
        ),
        // The long form is subject to the same rule.
        (
            "ucn_long_form",
            "int main(void){ const char *s = \"\\U00000041\"; return s[0]-65; }\n",
        ),
    ] {
        compile_expect_error(name, src, "not a valid universal character");
    }
}

/// The three carve-outs 6.4.3p2 names, and ordinary UCNs above 00A0.
#[test]
fn diagnostics_permitted_universal_character_names_are_accepted() {
    for (name, src) in [
        (
            "ucn_dollar",
            "int main(void){ const char *s = \"\\u0024\"; return s[0]-36; }\n",
        ),
        (
            "ucn_at",
            "int main(void){ const char *s = \"\\u0040\"; return s[0]-64; }\n",
        ),
        (
            "ucn_backtick",
            "int main(void){ const char *s = \"\\u0060\"; return s[0]-96; }\n",
        ),
        (
            "ucn_latin",
            "int main(void){ const char *s = \"\\u00e9\"; return s[0]!=0?0:1; }\n",
        ),
        (
            "ucn_ident_ok",
            "int \\u00c5ngstrom = 7;\nint main(void){ return 0; }\n",
        ),
        (
            "ucn_astral",
            "int main(void){ const char *s = \"\\U0001F600\"; return s[0]!=0?0:1; }\n",
        ),
        (
            "ucn_wide_char",
            "int main(void){ return (int)L'\\u00e9' - 233; }\n",
        ),
    ] {
        compile_expect_ok(name, src);
    }
}

// ============================================================================
// #C90 — sizeof of an incomplete type (C17 6.5.3.4p1)
// ============================================================================

/// `sizeof` shall not be applied to an incomplete type. An array is the
/// awkward case: the type table cannot tell `int[n]` from `int[]`, since both
/// simply have no extent. The size expressions decide it -- a level with an
/// expression is variably modified and so complete, a level without one is
/// incomplete -- which is what makes `sizeof(int[][n])`, two absent extents
/// against one expression, the incomplete array of arrays gcc rejects.
#[test]
fn diagnostics_sizeof_of_an_incomplete_type_is_rejected() {
    for (name, src) in [
        (
            "sz_arr_of_vla",
            "int main(void){ int n = 4; return (int)sizeof(int[][n]); }\n",
        ),
        (
            "sz_incomplete_arr",
            "int main(void){ return (int)sizeof(int[]); }\n",
        ),
        (
            "sz_arr_of_arr",
            "int main(void){ return (int)sizeof(int[][3]); }\n",
        ),
        (
            "sz_undef_struct",
            "struct U;\nint main(void){ return (int)sizeof(struct U); }\n",
        ),
        (
            "sz_undef_union",
            "union U;\nint main(void){ return (int)sizeof(union U); }\n",
        ),
        (
            "sz_undef_enum",
            "enum E;\nint main(void){ return (int)sizeof(enum E); }\n",
        ),
    ] {
        compile_expect_error(name, src, "incomplete type");
    }
}

/// Everything `sizeof` must still accept, including the two GNU extensions
/// gcc allows (`void` and a function type, both 1) and every complete array
/// shape -- without these the check could pass by refusing every array.
#[test]
fn diagnostics_sizeof_of_complete_types_is_accepted() {
    compile_expect_ok("sz_int", "int main(void){ return (int)sizeof(int) - 4; }\n");
    compile_expect_ok(
        "sz_ptr",
        "int main(void){ return (int)sizeof(int*) - 8; }\n",
    );
    compile_expect_ok(
        "sz_fixed_arr",
        "int main(void){ return (int)sizeof(int[4]) - 16; }\n",
    );
    compile_expect_ok(
        "sz_vla",
        "int main(void){ int n = 4; return (int)sizeof(int[n]) - 16; }\n",
    );
    compile_expect_ok(
        "sz_vla_2d",
        "int main(void){ int n = 4; return (int)sizeof(int[3][n]) - 48; }\n",
    );
    compile_expect_ok(
        "sz_ptr_to_vla",
        "int main(void){ int n = 4; return (int)sizeof(int(*)[n]) - 8; }\n",
    );
    compile_expect_ok(
        "sz_defined_struct",
        "struct S { int a; };\nint main(void){ return (int)sizeof(struct S) - 4; }\n",
    );
    compile_expect_ok(
        "sz_completed_enum",
        "enum E;\nenum E { A };\nint main(void){ return (int)sizeof(enum E) - 4; }\n",
    );
    // GNU extensions gcc accepts, both giving 1.
    compile_expect_ok(
        "sz_void",
        "int main(void){ return (int)sizeof(void) - 1; }\n",
    );
}

/// `typeof` yields a bare type, so a VLA's extent does not survive it and the
/// result is indistinguishable from an incomplete array. `sizeof(typeof(a))`
/// is legal -- gcc answers with the VLA's size -- so the completeness check
/// above must not fire on it. It answers 0 rather than 16, which is #C89 and
/// unfixed; what this pins is that it is not *rejected*, since `typeof`
/// appears in real system headers.
#[test]
fn diagnostics_sizeof_of_a_typeof_is_not_rejected() {
    // Was `* 0`, written to accommodate the wrong answer #C89 recorded: this
    // gave 0 where gcc gives 16. It is the real size now, so the arithmetic
    // can be the check.
    compile_expect_ok(
        "sz_typeof_vla",
        "int main(void){ int n = 4; int a[n]; return (int)sizeof(typeof(a)) - 16; }\n",
    );
    compile_expect_ok(
        "sz_typeof_fixed",
        "int main(void){ int b[4]; return (int)sizeof(typeof(b)) - 16; }\n",
    );
    compile_expect_ok(
        "sz_typeof_scalar",
        "int main(void){ int x = 0; return (int)sizeof(typeof(x)) - 4; }\n",
    );
}
