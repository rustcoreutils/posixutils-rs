use crate::test_compile::{
    compile, compile_expect_error, compile_expect_ok, compile_expect_warning,
};

// ============================================================================
// #C116 — the lock-free atomic ceiling
// ============================================================================

/// An `_Atomic` object c17 cannot access lock-free falls back to an ordinary,
/// non-atomic access, and must say so.
///
/// Nothing in the suite asserted this text, so the ceiling could have moved --
/// in either direction -- without a test noticing. It is load-bearing: gcc
/// emits `__atomic_*` calls above it, which need `-latomic`, and c17 hands the
/// link to the host `cc` without it (#X1).
///
/// The 3-byte struct is the case worth naming. It is *under* eight bytes and
/// still not lock-free, because the hardware has no 3-byte atomic -- so the
/// rule is "at a machine width", not "small enough".
#[test]
fn diagnostics_non_lock_free_atomic_warns() {
    for (name, src) in [
        (
            "atomic_oversized_struct",
            "struct Big { int a, b, c; };\n_Atomic struct Big g;\nvoid f(struct Big v) { g = v; }\n",
        ),
        (
            "atomic_odd_width_struct",
            "struct Odd { char a, b, c; };\n_Atomic struct Odd g;\nvoid f(struct Odd v) { g = v; }\n",
        ),
        (
            "atomic_long_double",
            "_Atomic long double g;\nvoid f(long double v) { g = v; }\n",
        ),
    ] {
        compile_expect_warning(name, src, "is not atomic");
    }
}

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
    let run = compile("atomic_lock_free_aggregate_silent", src, &[]);
    assert!(run.success, "should compile: {}", run.stderr);
    assert!(
        !run.stderr.contains("is not atomic"),
        "an aggregate at a lock-free width must not warn, got:\n{}",
        run.stderr
    );
}

/// A floating constant has no address, so a memory-class inline-asm
/// constraint cannot be satisfied. gcc says "memory input 0 is not directly
/// addressable" and stops; c17 reached `loc_to_asm_string` and panicked.
///
/// This is the one inline-asm constraint that has to be *rejected* rather than
/// materialized, and it is diagnosed in the backend -- the operand's actual
/// location is only known once registers are allocated -- so it also pins the
/// post-codegen error checkpoint that makes a backend diagnostic fail the
/// compile instead of writing the object anyway.
#[test]
fn diagnostics_float_constant_cannot_satisfy_a_memory_asm_constraint() {
    let src = r#"
int main(void) { __asm__ ("nop" :: "m"(1.0)); return 0; }
"#;
    compile_expect_error(
        "asm_float_const_memory_constraint",
        src,
        "not directly addressable",
    );
}

/// Every other constraint class accepts one: a general register takes the bit
/// pattern, an immediate substitutes it, and an SSE register gets it loaded.
/// Without this the test above would pass against a compiler that rejected
/// every floating asm operand.
#[cfg(target_arch = "x86_64")]
#[test]
fn diagnostics_float_constant_is_accepted_by_the_other_asm_classes() {
    for (name, constraint) in [
        ("asm_float_const_ok_r", "r"),
        ("asm_float_const_ok_i", "i"),
        ("asm_float_const_ok_g", "g"),
        ("asm_float_const_ok_x", "x"),
    ] {
        let src =
            format!("int main(void) {{ __asm__ (\"nop\" :: \"{constraint}\"(1.0)); return 0; }}\n");
        compile_expect_ok(name, &src);
    }
}

/// Only the reserved scratch registers are free across an asm body, and c17
/// has two of them (Xmm15 and Xmm14). A third SSE-class operand would have to
/// share one, silently overwriting a value, so it is refused instead.
///
/// The budget is shared by inputs and outputs: an `"=x"` output spends one,
/// leaving one for the inputs.
#[cfg(target_arch = "x86_64")]
#[test]
fn diagnostics_sse_asm_operands_are_limited_to_the_scratch_registers() {
    // Two fit.
    compile_expect_ok(
        "asm_two_sse_constants",
        "int main(void) { __asm__ (\"nop\" :: \"x\"(1.0), \"x\"(2.0)); return 0; }\n",
    );
    // A third does not.
    compile_expect_error(
        "asm_three_sse_constants",
        "int main(void) { __asm__ (\"nop\" :: \"x\"(1.0), \"x\"(2.0), \"x\"(3.0)); return 0; }\n",
        "too many SSE register constraints",
    );
    // An output spends one of the two, so two more inputs are one too many.
    compile_expect_error(
        "asm_sse_output_plus_two_inputs",
        "double r;\nint main(void) { __asm__ (\"nop\" : \"=x\"(r) : \"x\"(1.0), \"x\"(2.0)); return 0; }\n",
        "too many SSE register constraints",
    );
}
