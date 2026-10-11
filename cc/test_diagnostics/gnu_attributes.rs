//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc 13's diagnostics for the GNU attributes with arguments or a place of
// their own: `tls_model`, `retain`, `format_arg`, `flatten`, `noipa`,
// `zero_call_used_regs`, `patchable_function_entry` and
// `no_caller_saved_registers`.
//

use crate::test_compile::compile;

/// Each diagnostic of `src` compiled with `flags`, as `severity: message`,
/// and whether it compiled.
fn messages(name: &str, src: &str, flags: &[&str]) -> (bool, Vec<String>) {
    let c = compile(name, src, flags);
    let lines = c
        .stderr
        .lines()
        .filter(|l| !l.starts_with("c17: "))
        .map(|l| {
            let at = ["warning: ", "error: ", "note: "]
                .iter()
                .filter_map(|k| l.find(&format!(": {k}")))
                .min();
            l[at.map_or(0, |i| i + 2)..].to_string()
        })
        .collect();
    (c.success, lines)
}

/// `format_arg(N)` -- glibc's `__attribute_format_arg__` on `gettext` and
/// its kin -- is a function type's attribute, as `format` is: silent on a
/// function, a pointer to one or a typedef of one, and ignored with gcc's
/// warning on anything else.
#[test]
fn format_arg_is_a_function_type_attribute() {
    let src = r#"extern char *gettext(const char *) __attribute__((__format_arg__(1)));
extern char *dgettext(const char *, const char *) __attribute__((format_arg(2)));
typedef char *xlate(const char *) __attribute__((format_arg(1)));
char *(*pg)(const char *) __attribute__((format_arg(1)));
int bad __attribute__((format_arg(1)));
"#;
    let (ok, got) = messages("format_arg", src, &[]);
    assert!(ok, "{got:#?}");
    assert_eq!(
        got,
        ["warning: 'format_arg' attribute only applies to function types [-Wattributes]"]
    );
}

/// `retain` keeps a function or an object defined with static storage: on
/// a member, a parameter, a typedef, an `extern` object or an automatic
/// variable gcc ignores it with a warning; on a function's declaration it
/// waits for the definition, in silence.
#[test]
fn retain_misuse() {
    let src = r#"struct S { int m __attribute__((retain)); };
void fp(int p __attribute__((retain)));
typedef int T __attribute__((retain));
extern int later __attribute__((retain));
void f(void) __attribute__((retain));
void g(void) {
    static int s __attribute__((retain)) = 1;
    extern int e __attribute__((retain));
    int a __attribute__((retain)) = 0;
    (void)a; (void)s;
}
"#;
    let (ok, got) = messages("retain_misuse", src, &[]);
    assert!(ok, "{got:#?}");
    assert_eq!(
        got,
        vec!["warning: 'retain' attribute ignored [-Wattributes]"; 6]
    );
}

/// `flatten` and `noipa` are a function's own: anywhere else gcc ignores
/// them with a warning.
#[test]
fn function_only_attributes_of_this_phase() {
    for attr in ["flatten", "noipa", "no_profile_instrument_function"] {
        let src = format!(
            "int v __attribute__(({attr}));\n\
             typedef int T __attribute__(({attr}));\n\
             void f(void) __attribute__(({attr}));\n"
        );
        let (ok, got) = messages("fn_only_attr", &src, &[]);
        assert!(ok, "{attr}: {got:#?}");
        assert_eq!(
            got,
            vec![format!("warning: '{attr}' attribute ignored [-Wattributes]"); 2],
            "{attr}"
        );
    }
}

/// `zero_call_used_regs` and `patchable_function_entry` argument checks,
/// and where gcc 13 refuses or ignores them, in its words. Silent on a
/// function, and `patchable_function_entry` silent anywhere.
#[test]
fn code_generation_attribute_misuse() {
    let src = r#"__attribute__((zero_call_used_regs("bogus"))) int z1(int a) { return a; }
__attribute__((zero_call_used_regs(3))) int z2(int a) { return a; }
__attribute__((zero_call_used_regs)) int z3(int a) { return a; }
int zv __attribute__((zero_call_used_regs("all")));
__attribute__((patchable_function_entry(-1))) int p2(int a) { return a; }
__attribute__((patchable_function_entry("x"))) int p3(int a) { return a; }
__attribute__((patchable_function_entry)) int p4(int a) { return a; }
__attribute__((patchable_function_entry(1, 2, 3))) int p5(int a) { return a; }
int pv __attribute__((patchable_function_entry(2)));
__attribute__((patchable_function_entry(70000))) int p6(int a) { return a; }
__attribute__((patchable_function_entry(2, 3))) int p7(int a) { return a; }
__attribute__((zero_call_used_regs("used-gpr"), patchable_function_entry(2, 1))) int ok(int a) { return a; }
"#;
    // x86-64's: c17 has no aarch64 `-fzero-call-used-regs=`, and on aarch64
    // `zero_call_used_regs` is no attribute.
    let (ok, got) = messages(
        "codegen_attr_misuse",
        src,
        &["--target=x86_64-unknown-linux-gnu"],
    );
    assert!(!ok);
    assert_eq!(
        got,
        [
            "error: unrecognized 'zero_call_used_regs' attribute argument 'bogus'",
            "error: 'zero_call_used_regs' argument not a string",
            "error: wrong number of arguments specified for 'zero_call_used_regs' attribute",
            "note: expected 1, found 0",
            "error: 'zero_call_used_regs' attribute applies only to functions",
            "warning: 'patchable_function_entry' attribute argument '-1' is not an integer \
             constant [-Wattributes]",
            "warning: 'patchable_function_entry' attribute argument '\"x\"' is not an integer \
             constant [-Wattributes]",
            "error: wrong number of arguments specified for 'patchable_function_entry' attribute",
            "note: expected between 1 and 2, found 0",
            "error: wrong number of arguments specified for 'patchable_function_entry' attribute",
            "note: expected between 1 and 2, found 3",
            "warning: 'patchable_function_entry' attribute argument '70000' exceeds 65535 \
             [-Wattributes]",
            "warning: patchable function entry 3 exceeds size 2 [-Wattributes]",
        ]
    );
}

/// `no_caller_saved_registers` is x86-64's: a function type's attribute
/// there, which gcc 13 cannot compile while SSE is enabled, and on aarch64
/// no attribute at all.
#[test]
fn no_caller_saved_registers_needs_general_registers_only() {
    let src = "__attribute__((no_caller_saved_registers)) void f(void) {}\n\
               typedef void (*fp)(void) __attribute__((no_caller_saved_registers));\n\
               int v __attribute__((no_caller_saved_registers));\n";
    let (ok, got) = messages("ncsr_sse", src, &["--target=x86_64-unknown-linux-gnu"]);
    assert!(!ok);
    assert_eq!(
        got,
        [
            "error: sorry, unimplemented: SSE instructions aren't allowed in a function with \
             the 'no_caller_saved_registers' attribute",
            "warning: 'no_caller_saved_registers' attribute only applies to function types \
             [-Wattributes]",
        ]
    );
    let flags = ["--target=x86_64-unknown-linux-gnu", "-mgeneral-regs-only"];
    let (ok, got) = messages("ncsr_gpr", src, &flags);
    assert!(ok, "{got:#?}");
    assert_eq!(got.len(), 1, "{got:#?}");
    let (ok, got) = messages("ncsr_a64", src, &["--target=aarch64-unknown-linux-gnu"]);
    assert!(ok, "{got:#?}");
    assert_eq!(
        got,
        vec!["warning: 'no_caller_saved_registers' attribute directive ignored [-Wattributes]"; 3]
    );
}

/// `__has_attribute` gives gcc 13's answers in `#if` and in the program
/// text alike: the C standard's version for the three standard attributes
/// gcc also knows by their GNU names, 1 for the attributes this phase
/// implements, and 0 for those gcc 13 does not have.
#[test]
fn has_attribute_answers_in_both_evaluators() {
    let src = r#"
#if __has_attribute(deprecated) != 201904 || __has_attribute(__fallthrough__) != 201910
#error "standard attribute versions"
#endif
#if __has_attribute(noreturn) != 202202 || __has_attribute(__const)
#error "noreturn and __const"
#endif
#if !__has_attribute(tls_model) || !__has_attribute(__error__) || !__has_attribute(retain)
#error "implemented"
#endif
#if __has_attribute(counted_by) || __has_attribute(abi_tag) || __has_attribute(nonnull_if_nonzero)
#error "not in gcc 13"
#endif
#define HAS(x) __has_attribute(x)
_Static_assert(HAS(noreturn) == 202202, "noreturn");
_Static_assert(HAS(__deprecated__) == 201904, "deprecated");
_Static_assert(HAS(flatten) == 1 && HAS(noipa) == 1 && HAS(no_sanitize) == 1, "new");
_Static_assert(HAS(no_sanitize_memory) == 0, "no_sanitize_memory");
"#;
    let (ok, got) = messages("has_attribute_values", src, &[]);
    assert!(ok && got.is_empty(), "{got:#?}");
}

/// `__builtin_has_attribute` of a name that is no attribute is gcc's
/// error; a query may leave out the arguments a declaration must give.
#[test]
fn builtin_has_attribute_of_an_unknown_name() {
    let src = "int v;\n\
               int a = __builtin_has_attribute(v, bogus_attr);\n\
               int b = __builtin_has_attribute(v, cleanup) + __builtin_has_attribute(v, warning);\n";
    let (ok, got) = messages("has_attr_unknown", src, &[]);
    assert!(!ok);
    assert_eq!(got, ["error: unknown attribute 'bogus_attr'"]);
}

/// `no_sanitize("...")` and gcc 13's other sanitizer exemptions are a
/// function's, with gcc's argument checks: c17 inserts no sanitizer, so
/// on a function they change nothing and say nothing.
#[test]
fn no_sanitize_attributes() {
    let src = r#"int v1 __attribute__((no_sanitize("address")));
int v2 __attribute__((no_sanitize_undefined));
int v3 __attribute__((no_sanitize_coverage));
int v4 __attribute__((no_address_safety_analysis));
int v5 __attribute__((no_sanitize_address));
void f1(void) __attribute__((no_sanitize("address", "undefined,shift")));
void f2(void) __attribute__((no_sanitize));
void f3(void) __attribute__((no_sanitize(1)));
void f4(void) __attribute__((no_sanitize("bogus")));
void f5(void) __attribute__((no_sanitize_undefined, no_sanitize_coverage, no_address_safety_analysis));
typedef int T __attribute__((no_sanitize("address")));
"#;
    let (ok, got) = messages("no_sanitize", src, &[]);
    assert!(!ok);
    let ignored = |name: &str| format!("warning: '{name}' attribute ignored [-Wattributes]");
    assert_eq!(
        got,
        [
            ignored("no_sanitize"),
            ignored("no_sanitize_undefined"),
            ignored("no_sanitize_coverage"),
            ignored("no_address_safety_analysis"),
            ignored("no_sanitize_address"),
            "error: wrong number of arguments specified for 'no_sanitize' attribute".to_string(),
            "note: expected 1 or more, found 0".to_string(),
            "error: 'no_sanitize' argument not a string".to_string(),
            "warning: 'bogus' attribute directive ignored [-Wattributes]".to_string(),
            ignored("no_sanitize"),
        ]
    );
}

/// `uninitialized` exempts an automatic variable from
/// `-ftrivial-auto-var-init`; on anything else gcc 13 ignores it, saying
/// why. It was accepted everywhere in silence.
#[test]
fn uninitialized_misuse() {
    let src = r#"int g __attribute__((uninitialized));
static int s __attribute__((uninitialized));
void fn(void) __attribute__((uninitialized));
typedef int T __attribute__((uninitialized));
struct S { int m __attribute__((uninitialized)); };
int f(int p __attribute__((uninitialized))) {
    static int ls __attribute__((uninitialized));
    int a __attribute__((uninitialized));
    char buf[32] __attribute__((uninitialized));
    a = 1; (void)buf;
    return a + ls;
}
"#;
    let (ok, got) = messages("uninit_misuse", src, &["-ftrivial-auto-var-init=pattern"]);
    assert!(ok, "{got:#?}");
    let ignored = |name: &str, why: &str| {
        format!("warning: 'uninitialized' attribute ignored because '{name}' {why} [-Wattributes]")
    };
    let not_local = "is not a local variable";
    let not_var = "is not a variable";
    assert_eq!(
        got,
        [
            ignored("g", not_local),
            ignored("s", not_local),
            ignored("fn", not_var),
            ignored("T", not_var),
            ignored("m", not_var),
            ignored("p", not_var),
            ignored("ls", not_local),
        ]
    );
}

/// `tls_model` on anything but a thread-local, or with anything but one of
/// gcc's four model names, in gcc's words.
#[test]
fn tls_model_misuse() {
    let src = r#"int nt __attribute__((tls_model("initial-exec")));
extern int ne __attribute__((tls_model("local-exec")));
void fn(void) __attribute__((tls_model("initial-exec")));
typedef int T __attribute__((tls_model("initial-exec")));
struct S { int m __attribute__((tls_model("initial-exec"))); };
void fp(int p __attribute__((tls_model("local-exec"))));
int h(void) { int l __attribute__((tls_model("local-exec"))) = 0; return l; }
static __thread int st __attribute__((tls_model("initial-exec")));
int g(void) { static __thread int s __attribute__((tls_model("local-exec"))); return s + st; }
"#;
    let (ok, got) = messages("tls_model_misuse", src, &[]);
    assert!(ok, "{got:#?}");
    let ignored = |name: &str, why: &str| {
        format!("warning: 'tls_model' attribute ignored because '{name}' {why} [-Wattributes]")
    };
    let no_tls = "does not have thread storage duration";
    let not_var = "is not a variable";
    assert_eq!(
        got,
        [
            ignored("nt", no_tls),
            ignored("ne", no_tls),
            ignored("fn", not_var),
            ignored("T", not_var),
            ignored("m", not_var),
            ignored("p", not_var),
            ignored("l", no_tls),
        ]
    );
    let src = r#"__thread int bad1 __attribute__((tls_model("bogus")));
__thread int bad2 __attribute__((tls_model(3)));
__thread int bad3 __attribute__((tls_model));
__thread int bad4 __attribute__((tls_model("initial-exec", "local-exec")));
"#;
    let (ok, got) = messages("tls_model_args", src, &[]);
    assert!(!ok);
    assert_eq!(
        got,
        [
            "error: 'tls_model' argument must be one of 'local-exec', 'initial-exec', \
             'local-dynamic', or 'global-dynamic'",
            "error: 'tls_model' argument not a string",
            "error: wrong number of arguments specified for 'tls_model' attribute",
            "note: expected 1, found 0",
            "error: wrong number of arguments specified for 'tls_model' attribute",
            "note: expected 1, found 2",
        ]
    );
}
