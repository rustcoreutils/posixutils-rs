//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The `warning("msg")` and `error("msg")` function attributes: each call to
// such a function that survives optimization is reported, in gcc 13's
// words, so a call the optimizer proved dead is not -- the Linux kernel's
// BUILD_BUG_ON and compiletime_assert.
//

use crate::test_compile::compile;

/// Every diagnostic line of `src` compiled with `flags`, without the file
/// name, and without the driver's final line on failure.
fn diagnostics(name: &str, src: &str, flags: &[&str]) -> (bool, Vec<String>) {
    let c = compile(name, src, flags);
    let prefix = format!("{name}.c:");
    let lines = c
        .stderr
        .lines()
        .filter_map(|l| l.strip_prefix(&prefix))
        .map(str::to_string)
        .collect();
    (c.success, lines)
}

const CALLS: &str = r#"extern void bad(void) __attribute__((__error__("bad thing")));
extern void meh(void) __attribute__((__warning__("meh thing")));
extern void w2(int) __attribute__((warning("w2 msg")));
int f(int x) {
    if (sizeof(int) != 4)
        bad();
    if (x > 0 && x < 0)
        meh();
    meh();
    return x;
}
static inline void g(void) { meh(); }
int h(void) { g(); w2(1); return 0; }
"#;

/// At -O0 every call is reported but the one in a branch whose condition
/// is a constant, which is never generated; at -O2 the call in a branch the
/// optimizer folds is not, and a call inlined into another function is
/// reported once, where it was written.
#[test]
fn calls_that_survive_optimization_are_reported() {
    let (ok, got) = diagnostics("attr_calls_o0", CALLS, &["-O0"]);
    assert!(ok, "{got:#?}");
    assert_eq!(
        got,
        [
            "8:9: warning: call to 'meh' declared with attribute warning: meh thing [-Wattribute-warning]",
            "9:5: warning: call to 'meh' declared with attribute warning: meh thing [-Wattribute-warning]",
            "12:30: warning: call to 'meh' declared with attribute warning: meh thing [-Wattribute-warning]",
            "13:20: warning: call to 'w2' declared with attribute warning: w2 msg [-Wattribute-warning]",
        ]
    );
    let (ok, got) = diagnostics("attr_calls_o2", CALLS, &["-O2"]);
    assert!(ok, "{got:#?}");
    assert_eq!(
        got,
        [
            "9:5: warning: call to 'meh' declared with attribute warning: meh thing [-Wattribute-warning]",
            "12:30: warning: call to 'meh' declared with attribute warning: meh thing [-Wattribute-warning]",
            "13:20: warning: call to 'w2' declared with attribute warning: w2 msg [-Wattribute-warning]",
        ]
    );
}

/// An `error` call that survives is an error; one the optimizer removes is
/// nothing, so the same unit is refused at -O0 and compiles at -O2.
#[test]
fn an_error_call_in_dead_code_is_not_reported() {
    let src = "extern void bad(void) __attribute__((error(\"bad thing\")));\n\
               int f(int x) {\n\
               \x20   if (x > 0 && x < 0)\n\
               \x20       bad();\n\
               \x20   return x;\n\
               }\n";
    let (ok, got) = diagnostics("attr_error_o0", src, &["-O0"]);
    assert!(!ok);
    assert_eq!(
        got,
        ["4:9: error: call to 'bad' declared with attribute error: bad thing"]
    );
    let (ok, got) = diagnostics("attr_error_o2", src, &["-O2"]);
    assert!(ok, "{got:#?}");
    assert!(got.is_empty(), "{got:#?}");
}

/// The kernel's compiletime_assert: a block-scope `extern` declaration of
/// a uniquely named `__error__` function, called when the condition fails.
/// Inlined with a constant, the call goes; left a call, it is the error.
#[test]
fn kernel_compiletime_assert() {
    let src = r#"#define ___PASTE(a, b) a##b
#define __compiletime_assert(condition, msg, prefix, suffix)            \
    do {                                                                \
        extern void prefix ## suffix(void) __attribute__((__error__(msg))); \
        if (!(condition))                                               \
            prefix ## suffix();                                         \
    } while (0)
#define _compiletime_assert(condition, msg, prefix, suffix) \
    __compiletime_assert(condition, msg, prefix, suffix)
#define BUILD_BUG_ON(cond) \
    _compiletime_assert(!(cond), "BUILD_BUG_ON failed: " #cond, __compiletime_assert_, __COUNTER__)
static inline int check(int n) { BUILD_BUG_ON(n > 8); return n * 2; }
int main(void) { BUILD_BUG_ON(sizeof(int) != 4); return check(4) - 8; }
"#;
    for opt in ["-O1", "-O2"] {
        let (ok, got) = diagnostics("attr_bug_on", src, &[opt]);
        assert!(ok && got.is_empty(), "{opt}: {got:#?}");
    }
    let (ok, got) = diagnostics("attr_bug_on_o0", src, &["-O0"]);
    assert!(!ok);
    assert_eq!(got.len(), 1, "{got:#?}");
    assert!(
        got[0].ends_with(
            ": error: call to '__compiletime_assert_0' declared with attribute error: \
             BUILD_BUG_ON failed: n > 8"
        ),
        "{got:#?}"
    );
}

/// Both attributes on one function are both reported, the error first; an
/// attribute on a declaration after the call, or after the definition,
/// still applies; and a call the inliner replaces is not reported.
#[test]
fn every_declaration_and_only_direct_calls() {
    let src = r#"void both(void) __attribute__((warning("W"), error("E")));
void later(void);
void def(void) __attribute__((warning("defined W")));
void def(void) {}
void after(void) {}
void f(void) {
    later();
    both();
    def();
    after();
}
void later(void) __attribute__((error("later E")));
void after(void) __attribute__((warning("after W")));
"#;
    let (ok, got) = diagnostics("attr_every_o0", src, &["-O0"]);
    assert!(!ok);
    assert_eq!(
        got,
        [
            "7:5: error: call to 'later' declared with attribute error: later E",
            "8:5: error: call to 'both' declared with attribute error: E",
            "8:5: warning: call to 'both' declared with attribute warning: W [-Wattribute-warning]",
            "9:5: warning: call to 'def' declared with attribute warning: defined W [-Wattribute-warning]",
            "10:5: warning: call to 'after' declared with attribute warning: after W [-Wattribute-warning]",
        ]
    );
    let (ok, got) = diagnostics("attr_every_o2", src, &["-O2"]);
    assert!(!ok);
    assert_eq!(
        got,
        [
            "7:5: error: call to 'later' declared with attribute error: later E",
            "8:5: error: call to 'both' declared with attribute error: E",
            "8:5: warning: call to 'both' declared with attribute warning: W [-Wattribute-warning]",
        ]
    );
}

/// A function renamed by an asm label is reported by its C name, as gcc
/// reports it: the call names the symbol, `g`, and the report said so.
#[test]
fn reports_name_the_c_identifier_not_the_asm_label() {
    let src = r#"int f(void) __asm__("g") __attribute__((error("x")));
int w(void) __asm__("w_impl") __attribute__((warning("y")));
__attribute__((warning("def"))) int d(void) __asm__("d_impl");
__attribute__((noinline)) int d(void) { return 1; }
int l(void) __asm__("l_impl");
int h(void) { return f() + w() + d() + l(); }
int l(void) __attribute__((error("late")));
int k(void) { extern int m(void) __asm__("m_impl") __attribute__((warning("blk"))); return m(); }
"#;
    for opt in ["-O0", "-O2"] {
        let (ok, got) = diagnostics("attr_asm_label", src, &[opt]);
        assert!(!ok);
        assert_eq!(
            got,
            [
                "6:22: error: call to 'f' declared with attribute error: x",
                "6:28: warning: call to 'w' declared with attribute warning: y [-Wattribute-warning]",
                "6:34: warning: call to 'd' declared with attribute warning: def [-Wattribute-warning]",
                "6:40: error: call to 'l' declared with attribute error: late",
                "8:92: warning: call to 'm' declared with attribute warning: blk [-Wattribute-warning]",
            ],
            "{opt}"
        );
    }
}

/// `-Wno-attribute-warning` silences the warnings, never the errors.
#[test]
fn warning_group_is_attribute_warning() {
    let src = "void w(void) __attribute__((warning(\"W\")));\n\
               void f(void) { w(); }\n";
    let (ok, got) = diagnostics("attr_wno", src, &["-Wno-attribute-warning"]);
    assert!(ok && got.is_empty(), "{got:#?}");
    let (ok, got) = diagnostics("attr_werror", src, &["-Werror=attribute-warning"]);
    assert!(!ok);
    assert_eq!(
        got,
        ["2:16: error: call to 'w' declared with attribute warning: W [-Werror=attribute-warning]"]
    );
}

/// Written anywhere but on a function, or with an argument that is not a
/// string, the attribute is ignored with gcc's warning; the wrong number of
/// arguments is gcc's error.
#[test]
fn misuse_is_diagnosed_as_gcc_does() {
    let src = r#"int v __attribute__((warning("on var")));
typedef void T(void) __attribute__((error("on typedef")));
void (*fp)(void) __attribute__((error("on ptr")));
void e1(void) __attribute__((error(1)));
void use(void) { e1(); fp(); }
"#;
    let (ok, got) = diagnostics("attr_call_misuse", src, &[]);
    assert!(ok, "{got:#?}");
    let messages: Vec<&str> = got
        .iter()
        .map(|l| &l[l.find(": warning: ").map_or(0, |i| i + 2)..])
        .collect();
    assert_eq!(
        messages,
        [
            "warning: 'warning' attribute ignored [-Wattributes]",
            "warning: 'error' attribute ignored [-Wattributes]",
            "warning: 'error' attribute ignored [-Wattributes]",
            "warning: 'error' attribute ignored [-Wattributes]",
        ]
    );
    for (attr, found) in [("error", "0"), ("warning(\"a\", \"b\")", "2")] {
        let src = format!("void e(void) __attribute__(({attr}));\n");
        let (ok, got) = diagnostics("attr_call_count", &src, &[]);
        assert!(!ok);
        let name = &attr[..attr.find('(').unwrap_or(attr.len())];
        let messages: Vec<&str> = got
            .iter()
            .map(|l| &l[l.find(": ").map_or(0, |i| i + 2)..])
            .collect();
        assert_eq!(
            messages,
            [
                format!("error: wrong number of arguments specified for '{name}' attribute"),
                format!("note: expected 1, found {found}"),
            ],
            "{attr}"
        );
    }
}
