use crate::test_compile::{compile_expect_error, compile_expect_ok, compile_expect_warning};

// ============================================================================
// #C116 — the lock-free atomic ceiling
// ============================================================================

/// Case ranges are checked for overlap, not just equality.
///
/// 6.8.4.2p3 forbids two equal case constants, and GCC extends that to
/// overlapping ranges. The check here was `Vec::contains` over individual
/// values; a range needs an interval test. Without it an overlapping arm
/// becomes silently unreachable, because the body walk resolves a label by
/// finding the first match.
#[test]
fn diagnostics_overlapping_case_ranges_are_rejected() {
    compile_expect_error(
        "case_range_overlap",
        "int f(int x){ switch(x){ case 1 ... 5: return 0; case 4 ... 9: return 1; } return 2; }\n\
         int main(void){ return f(0); }\n",
        "overlapping",
    );
    // A range that swallows a plain label is the same fault.
    compile_expect_error(
        "case_range_covers_single",
        "int f(int x){ switch(x){ case 1 ... 5: return 0; case 3: return 1; } return 2; }\n\
         int main(void){ return f(0); }\n",
        "case value",
    );
    // Adjacent ranges do not overlap and must be accepted.
    compile_expect_ok(
        "case_ranges_adjacent",
        "int f(int x){ switch(x){ case 1 ... 5: return 0; case 6 ... 9: return 1; } return 2; }\n\
         int main(void){ return f(0); }\n",
    );
}

/// An empty case range warns and never matches, as in GCC.
#[test]
fn diagnostics_empty_case_range_warns() {
    compile_expect_warning(
        "case_range_empty",
        "int f(int x){ switch(x){ case 9 ... 1: return 0; default: return 1; } }\n\
         int main(void){ return f(5); }\n",
        "empty range",
    );
}

/// Both endpoints of a range must be integer constant expressions.
#[test]
fn diagnostics_case_range_endpoints_must_be_constant() {
    compile_expect_error(
        "case_range_runtime_high",
        "int f(int x, int n){ switch(x){ case 1 ... n: return 0; } return 2; }\n\
         int main(void){ return f(0, 1); }\n",
        "constant expression",
    );
}

/// An array designator that addresses past the end of its array is rejected.
///
/// Nothing checked this anywhere: `int a[4] = {[10] = 7};` compiled and wrote
/// past the array, statically and at run time alike. GCC rejects it. Ranges
/// make it easy to write by accident, so the bound is checked where the array
/// size is known.
#[test]
fn diagnostics_designator_out_of_bounds() {
    compile_expect_error(
        "designator_past_end",
        "int a[4] = {[10] = 7};\nint main(void){ return a[0]; }\n",
        "exceeds array bounds",
    );
    compile_expect_error(
        "designator_range_past_end",
        "int a[4] = {[2 ... 9] = 7};\nint main(void){ return a[0]; }\n",
        "exceeds array bounds",
    );
    // An array sized *by* its initializer cannot overflow it.
    compile_expect_ok(
        "designator_infers_size",
        "int a[] = {[10] = 7};\nint main(void){ return a[10] == 7 ? 0 : 1; }\n",
    );
    // The last valid index is still valid.
    compile_expect_ok(
        "designator_last_index",
        "int a[4] = {[3] = 7};\nint main(void){ return a[3] == 7 ? 0 : 1; }\n",
    );
}

/// A reversed or negative index range is rejected, as in GCC.
#[test]
fn diagnostics_designator_range_is_well_formed() {
    compile_expect_error(
        "designator_range_reversed",
        "int a[4] = {[3 ... 1] = 5};\nint main(void){ return 0; }\n",
        "empty index range",
    );
    compile_expect_error(
        "designator_negative",
        "int a[4] = {[-1] = 5};\nint main(void){ return 0; }\n",
        "negative",
    );
    // A single-element range is well formed.
    compile_expect_ok(
        "designator_range_single",
        "int a[4] = {[1 ... 1] = 5};\nint main(void){ return a[1] == 5 ? 0 : 1; }\n",
    );
}

/// `&&label` naming a label the function never defines is an error.
///
/// The block minted for the reference stayed empty and unterminated, so
/// branching to it ran off the end of the function and the program hung.
/// Checked at the end of the function, because a forward reference is legal.
#[test]
fn diagnostics_label_address_must_name_a_label() {
    compile_expect_error(
        "label_addr_undefined",
        "int main(void){ void *p = &&nowhere; goto *p; return 1; }\n",
        "used but not defined",
    );
    // A forward reference is fine.
    compile_expect_ok(
        "label_addr_forward",
        "int main(void){ void *p = &&L; goto *p; return 1; L: return 0; }\n",
    );
}

/// Every way of naming a label -- `goto`, `&&label`, `asm goto` -- is checked
/// by one rule, wherever the reference sits. A `goto` inside a statement
/// expression escaped the check, which walked statements only, and ran off the
/// end of the function; one between case labels must still be caught.
#[test]
fn diagnostics_every_label_reference_must_name_a_label() {
    compile_expect_error(
        "goto_undefined_in_stmt_expr",
        "int f(void){ return ({ goto nowhere; 1; }); }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "goto_undefined_in_switch",
        "int f(int a){ switch (a) { case 0: goto nowhere; } return 0; }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "asm_goto_undefined",
        "int f(void){ asm goto(\"\" :::: nowhere); return 0; }\n",
        "label 'nowhere' used but not defined",
    );
    compile_expect_error(
        "label_addr_undefined_in_switch",
        "void *p;\nvoid f(int a){ switch (a) { case 0: p = &&nowhere; c1: a = 2; } }\n",
        "label 'nowhere' used but not defined",
    );
    // Labels in a switch body and in a statement expression are found by
    // both kinds of reference.
    compile_expect_ok(
        "labels_in_switch_and_stmt_expr",
        "void *p;\nint f(int a){\n  switch (a) { case 0: p = &&c1; goto c1; c1: a = 2; a1: case 1: a = 3; }\n  \
         return ({ p = &&se; goto se; se: ; a; });\n}\nint main(void){ return f(0) == 3 ? 0 : 1; }\n",
    );
}

/// `&&label` outside any function is an error, not a compiler crash.
#[test]
fn diagnostics_label_address_outside_a_function() {
    compile_expect_error(
        "label_addr_file_scope",
        "void *g = &&L;\nint main(void){ L: return 0; }\n",
        "outside of any function",
    );
}

/// The operand of a computed goto must be a pointer.
///
/// An integer is scalar, so testing scalarity accepted `goto *3;` — and the
/// 64-bit store of a 32-bit value then branched through a half-initialised
/// address.
#[test]
fn diagnostics_computed_goto_requires_a_pointer() {
    compile_expect_error(
        "computed_goto_int",
        "int main(void){ int n = 3; goto *n; return 1; }\n",
        "must be a pointer",
    );
    compile_expect_error(
        "computed_goto_double",
        "int main(void){ double d = 1.0; goto *d; return 1; }\n",
        "must be a pointer",
    );
}

/// An index range after a field designator is refused, not silently dropped.
///
/// `.m[0 ... 3] = v` resolves through the designator chain, which yields one
/// offset where a range names many, so it initialized nothing at all and said
/// nothing about it. The nested spelling does the same job.
#[test]
fn diagnostics_index_range_after_field_designator() {
    compile_expect_error(
        "range_after_field",
        "struct S { int m[4]; int t; };\nstruct S s = { .m[0 ... 3] = 7, .t = 9 };\n\
         int main(void){ return s.m[0]; }\n",
        "index range is not supported after a field designator",
    );
    // The nested form works and is what the diagnostic points at.
    compile_expect_ok(
        "range_nested_in_field",
        "struct S { int m[4]; int t; };\nstruct S s = { .m = { [0 ... 3] = 7 }, .t = 9 };\n\
         int main(void){ return (s.m[3] == 7 && s.t == 9) ? 0 : 1; }\n",
    );
}
