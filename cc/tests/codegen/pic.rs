//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Position-Independent Code (PIC) Mega-Test
//
// Consolidates: ALL pic tests
//
// PIC is required for shared libraries and enables code to be loaded at any
// address without relocation. Key aspects:
// - External symbols are accessed via the Global Offset Table (GOT)
// - Function calls may use the Procedure Linkage Table (PLT)
// - All data references use RIP-relative addressing on x86-64
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: PIC code generation
// ============================================================================

/// Also covers `codegen_pic_flags`'s -fPIC (uppercase) case, whose program
/// was an empty `main`: this one is built with exactly `-fPIC`.
#[test]
fn codegen_pic_mega() {
    let code = r#"
extern unsigned long strlen(const char *s);

int global_value = 42;
int array[5] = {1, 2, 3, 4, 5};
int global_state = 0;

int increment(void) {
    static int counter = 0;
    return ++counter;
}

int add(int a, int b) { return a + b; }
int mul(int a, int b) { return a * b; }

int apply(int (*fn)(int, int), int x, int y) {
    return fn(x, y);
}

int sum_array(void) {
    int total = 0;
    for (int i = 0; i < 5; i++) {
        total += array[i];
    }
    return total;
}

const char *get_hello(void) {
    return "hello";
}

const char *get_world(void) {
    return "world";
}

typedef int (*operation)(int, int);

struct Calculator {
    operation add;
    operation sub;
};

int do_add(int a, int b) { return a + b; }
int do_sub(int a, int b) { return a - b; }

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

void set_state(int val) { global_state = val; }
int get_state(void) { return global_state; }

int indirect_call(void (*setter)(int), int (*getter)(void), int val) {
    setter(val);
    return getter();
}

int main(void) {
    // ========== BASIC PIC (returns 1-9) ==========

    // Global variable access
    if (global_value != 42) return 1;
    global_value = 100;
    if (global_value != 100) return 2;
    global_value = 42;  // Reset

    // ========== STATIC LOCAL (returns 10-19) ==========
    {
        if (increment() != 1) return 10;
        if (increment() != 2) return 11;
        if (increment() != 3) return 12;
    }

    // ========== FUNCTION POINTERS (returns 20-29) ==========
    {
        if (apply(add, 3, 4) != 7) return 20;
        if (apply(mul, 3, 4) != 12) return 21;
    }

    // ========== GLOBAL ARRAY (returns 30-39) ==========
    {
        if (sum_array() != 15) return 30;
        array[0] = 10;
        if (sum_array() != 24) return 31;
        array[0] = 1;  // Reset
    }

    // ========== STRING LITERALS (returns 40-49) ==========
    {
        const char *h = get_hello();
        const char *w = get_world();
        if (h[0] != 'h') return 40;
        if (w[0] != 'w') return 41;
    }

    // ========== STRUCT WITH FUNC PTR (returns 50-59) ==========
    {
        struct Calculator calc = { do_add, do_sub };
        if (calc.add(10, 5) != 15) return 50;
        if (calc.sub(10, 5) != 5) return 51;
    }

    // ========== RECURSION (returns 60-69) ==========
    {
        if (factorial(5) != 120) return 60;
        if (factorial(6) != 720) return 61;
    }

    // ========== EXTERNAL SYMBOL (returns 70-79) ==========
    {
        if (strlen("hello") != 5) return 70;
        if (strlen("") != 0) return 71;
        if (strlen("test string") != 11) return 72;
    }

    // ========== COMPLEX CALLS (returns 80-89) ==========
    {
        set_state(42);
        if (get_state() != 42) return 80;

        if (indirect_call(set_state, get_state, 100) != 100) return 81;
        if (global_state != 100) return 82;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_pic_mega", code, &["-fPIC".to_string()]),
        0
    );
}

/// Test both -fPIC (uppercase) and -fpic (lowercase) flags
///
/// The -fPIC half (`pic_uppercase`, an empty `main`) is `codegen_pic_mega`,
/// which builds and runs with exactly that flag.
#[test]
fn codegen_pic_flags() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pic_lowercase", code, &["-fpic".to_string()]),
        0
    );
}

/// Test PIE code generation (-fPIE / -fpie)
#[test]
fn codegen_pie_flags() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pie_uppercase", code, &["-fPIE".to_string()]),
        0
    );
    assert_eq!(
        compile_and_run("pie_lowercase", code, &["-fpie".to_string()]),
        0
    );
}

/// Test PIE disable flag (-fno-pie)
#[test]
fn codegen_pie_disable_flag() {
    let code = r#"
int main(void) {
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("pie_disable", code, &["-fno-pie".to_string()]),
        0
    );
}

/// Hidden objects defined in another unit and reached PC-relative, and a
/// static and a hidden function's address, link and read back in a `-fPIC`
/// build, as the GOT-free access must.
#[test]
fn codegen_pic_hidden_symbols_across_units() {
    let unit_a = r#"
extern int hv __attribute__((visibility("hidden")));
extern int hv_arr[4] __attribute__((visibility("hidden")));
extern int hfn(int) __attribute__((visibility("hidden")));
static int sv = 3;
static int sfn(int x) { return x + sv; }
int main(void)
{
    int (*volatile fp)(int) = hfn;
    int (*volatile sp)(int) = sfn;
    hv_arr[2] = 9;
    return !(hv == 5 && fp(1) == 2 && sp(1) == 4 && hv_arr[2] == 9 && &hv_arr[3] - &hv_arr[0] == 3);
}
"#;
    let unit_b = r#"
__attribute__((visibility("hidden"))) int hv = 5;
__attribute__((visibility("hidden"))) int hv_arr[4];
__attribute__((visibility("hidden"))) int hfn(int x) { return x + 1; }
"#;
    for opt in ["-O0", "-O2"] {
        let opts = vec!["-fPIC".to_string(), opt.to_string()];
        assert_eq!(
            crate::common::compile_and_run_two_units("pic_hidden_units", unit_a, unit_b, &opts),
            0,
            "{opt}"
        );
    }
}

/// `#pragma GCC visibility push(hidden)` around `extern` declarations, as
/// Linux force-includes it into its PIE decompressor: the object built
/// `-fPIE` reaches them with no GOT relocation, and the program links and
/// runs against the unit that defines them under the same pragma.
#[cfg(target_os = "linux")]
#[test]
fn codegen_pie_visibility_pragma_needs_no_got() {
    let unit_a = r#"
#pragma GCC visibility push(hidden)
extern int hv;
extern int harr[4];
extern int hfn(int);
#pragma GCC visibility pop
int main(void)
{
    int (*volatile fp)(int) = hfn;
    harr[1] = 7;
    return !(hv == 5 && fp(1) == 2 && harr[1] == 7);
}
"#;
    let unit_b = r#"
#pragma GCC visibility push(hidden)
int hv = 5;
int harr[4];
int hfn(int x) { return x + 1; }
#pragma GCC visibility pop
"#;
    let dir = crate::common::work_dir("vis_pragma_pie");
    for opt in ["-O0", "-O2"] {
        let obj =
            crate::common::c17_object_with("vis_pragma_main", unit_a, opt, &["-fPIE"], dir.path());
        let relocs = std::process::Command::new("readelf")
            .args(["-rW", &obj])
            .output()
            .expect("readelf");
        let relocs = String::from_utf8_lossy(&relocs.stdout);
        assert!(
            !relocs.contains("GOT"),
            "{opt}: GOT relocation in:\n{relocs}"
        );
        let opts = vec!["-fPIE".to_string(), opt.to_string()];
        assert_eq!(
            crate::common::compile_and_run_two_units("vis_pragma_units", unit_a, unit_b, &opts),
            0,
            "{opt}"
        );
    }
}

/// A shared object built `-fvisibility=hidden` that takes the address of a
/// `gnu_inline` function defined out of line in another of its units links
/// and runs: the inline body is no definition, so the address comes from
/// the GOT. Taken PC-relative, as for a hidden symbol, it made guile's
/// library unlinkable ("relocation R_X86_64_PC32 against symbol `scm_car'
/// can not be used when making a shared object").
#[cfg(target_os = "linux")]
#[test]
fn codegen_pic_fvisibility_inline_definition_address() {
    let header = r#"
extern __inline__ __attribute__((__gnu_inline__)) int scm_car(int x) { return x + 1; }
"#;
    let user = format!(
        "{header}int (*p)(int);\n\
         __attribute__((visibility(\"default\"))) int lib_call(int x) {{ p = scm_car; return p(x); }}\n"
    );
    let definer = "__attribute__((visibility(\"default\"))) int scm_car(int x) { return x + 1; }\n";
    let main = "int lib_call(int);\nint main(void) { return lib_call(41) != 42; }\n";
    let dir = crate::common::work_dir("pic_fvis_inline");
    let p = |name: &str| dir.path().join(name).to_string_lossy().into_owned();
    std::fs::write(p("user.c"), user).unwrap();
    std::fs::write(p("definer.c"), definer).unwrap();
    std::fs::write(p("main.c"), main).unwrap();
    for opt in ["-O0", "-O2"] {
        let r = crate::common::run_c17(&[
            opt,
            "-shared",
            "-fPIC",
            "-fvisibility=hidden",
            "-o",
            &p("libfvis.so"),
            &p("user.c"),
            &p("definer.c"),
        ]);
        assert!(r.success, "{opt}: {}", r.stderr);
        let r = crate::common::run_c17(&[
            "-o",
            &p("main"),
            &p("main.c"),
            &p("libfvis.so"),
            &format!("-Wl,-rpath,{}", dir.path().display()),
        ]);
        assert!(r.success, "{opt}: {}", r.stderr);
        let code = std::process::Command::new(p("main"))
            .status()
            .unwrap()
            .code();
        assert_eq!(code, Some(0), "{opt}");
    }
}

