//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline assembly and attributes: asm operands, aliases, weak and
// hidden symbols, constructors and destructors.
//

use crate::common::{compile_and_run, compile_and_run_everywhere, compile_and_run_two_units};
// Only the x86-64 tests below assemble and run a file of their own.
#[cfg(target_arch = "x86_64")]
use std::io::Write;
#[cfg(target_arch = "x86_64")]
use std::process::Command;

// ============================================================================
// Assembly file compilation tests (.s and .S)
// ============================================================================

/// Create a temporary assembly file
#[cfg(target_arch = "x86_64")]
fn create_asm_file(name: &str, content: &str, extension: &str) -> plib::tmp::NamedTempFile {
    let mut file = plib::tmp::Builder::new()
        .prefix(&format!("c17_test_{}_", name))
        .suffix(extension)
        .tempfile()
        .expect("failed to create temp file");
    file.write_all(content.as_bytes())
        .expect("failed to write test file");
    file
}

#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_asm_file_support() {
    // Test .s file (pure assembly) and .S file (assembly with preprocessor)
    // Tests compile-only (-c) mode which fully works. Mixing C + asm in one
    // invocation requires passing asm objects to C file's link step (future work).

    // Assembly function that returns 42 (x86_64)
    // Note: macOS Mach-O uses underscore prefix and has no .type/.size directives
    #[cfg(target_os = "macos")]
    let asm_content = r#"
    .text
    .globl _get_value
_get_value:
    movl $42, %eax
    ret
"#;

    #[cfg(not(target_os = "macos"))]
    let asm_content = r#"
    .text
    .globl get_value
    .type get_value, @function
get_value:
    movl $42, %eax
    ret
    .size get_value, .-get_value
"#;

    // Assembly with preprocessor directives (.S) (x86_64)
    #[cfg(target_os = "macos")]
    let asm_s_content = r#"
#define RETURN_VALUE 99
    .text
    .globl _get_value_s
_get_value_s:
    movl $RETURN_VALUE, %eax
    ret
"#;

    #[cfg(not(target_os = "macos"))]
    let asm_s_content = r#"
#define RETURN_VALUE 99
    .text
    .globl get_value_s
    .type get_value_s, @function
get_value_s:
    movl $RETURN_VALUE, %eax
    ret
    .size get_value_s, .-get_value_s
"#;

    // C main that calls both functions
    let c_content = r#"
extern int get_value(void);
extern int get_value_s(void);

int main(void) {
    if (get_value() != 42) return 1;
    if (get_value_s() != 99) return 2;
    return 0;
}
"#;

    let asm_file = create_asm_file("asm_test", asm_content, ".s");
    let asm_s_file = create_asm_file("asm_s_test", asm_s_content, ".S");
    let c_file = crate::common::create_c_file("asm_main", c_content);

    let work = crate::common::work_dir("asm_test");
    let obj_s = work.path().join("asm.o");
    let obj_s_upper = work.path().join("asm_s.o");
    let obj_c = work.path().join("asm_c.o");
    let exe_path = work.path().join("asm_test");

    // Step 1: Compile .s to .o
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-c".to_string(),
            "-o".to_string(),
            obj_s.to_string_lossy().to_string(),
            asm_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 -c .s failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 2: Compile .S to .o (with preprocessing)
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-c".to_string(),
            "-o".to_string(),
            obj_s_upper.to_string_lossy().to_string(),
            asm_s_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 -c .S failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 3: Compile C to .o
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-c".to_string(),
            "-o".to_string(),
            obj_c.to_string_lossy().to_string(),
            c_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 -c .c failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 4: Link all .o files
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-o".to_string(),
            exe_path.to_string_lossy().to_string(),
            obj_c.to_string_lossy().to_string(),
            obj_s.to_string_lossy().to_string(),
            obj_s_upper.to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 link failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Run the executable
    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");

    let exit_code = run_output.status.code().unwrap_or(-1);

    assert_eq!(
        exit_code, 0,
        "Assembly file test failed with exit code {}",
        exit_code
    );
}

/// Test that __ASSEMBLER__ is defined when preprocessing .S files
#[cfg(target_arch = "x86_64")]
#[test]
fn codegen_asm_assembler_macro() {
    // Test that __ASSEMBLER__ is defined in .S files and that
    // #ifdef __ASSEMBLER__ conditional compilation works

    // Assembly with __ASSEMBLER__ conditional
    #[cfg(target_os = "macos")]
    let asm_s_content = r#"
#ifdef __ASSEMBLER__
#define RETURN_VALUE 77
#else
#error "__ASSEMBLER__ should be defined"
#endif
    .text
    .globl _get_asm_value
_get_asm_value:
    movl $RETURN_VALUE, %eax
    ret
"#;

    #[cfg(not(target_os = "macos"))]
    let asm_s_content = r#"
#ifdef __ASSEMBLER__
#define RETURN_VALUE 77
#else
#error "__ASSEMBLER__ should be defined"
#endif
    .text
    .globl get_asm_value
    .type get_asm_value, @function
get_asm_value:
    movl $RETURN_VALUE, %eax
    ret
    .size get_asm_value, .-get_asm_value
"#;

    // C main that calls the asm function
    let c_content = r#"
extern int get_asm_value(void);

int main(void) {
    if (get_asm_value() != 77) return 1;
    return 0;
}
"#;

    let asm_s_file = create_asm_file("asm_assembler_test", asm_s_content, ".S");
    let c_file = crate::common::create_c_file("asm_assembler_main", c_content);

    let work = crate::common::work_dir("asm_macro");
    let obj_asm = work.path().join("asm_macro.o");
    let obj_c = work.path().join("asm_macro_c.o");
    let exe_path = work.path().join("asm_macro_test");

    // Step 1: Compile .S to .o (with preprocessing, should have __ASSEMBLER__ defined)
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-c".to_string(),
            "-o".to_string(),
            obj_asm.to_string_lossy().to_string(),
            asm_s_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 -c .S with __ASSEMBLER__ failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 2: Compile C to .o
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-c".to_string(),
            "-o".to_string(),
            obj_c.to_string_lossy().to_string(),
            c_file.path().to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 -c .c failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Step 3: Link
    let output = plib::testing::run_test_base(
        "c17",
        &[
            "-o".to_string(),
            exe_path.to_string_lossy().to_string(),
            obj_c.to_string_lossy().to_string(),
            obj_asm.to_string_lossy().to_string(),
        ],
        &[],
    );
    assert!(
        output.status.success(),
        "c17 link failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Run the executable
    let run_output = Command::new(&exe_path)
        .output()
        .expect("failed to run executable");

    let exit_code = run_output.status.code().unwrap_or(-1);

    assert_eq!(
        exit_code, 0,
        "__ASSEMBLER__ macro test failed with exit code {}",
        exit_code
    );
}

/// `constructor` and `destructor` run, in the order gcc runs them.
///
/// Ordering is the whole point of the priority argument, so the test records a
/// trace rather than a "did it run" flag. The expectations were taken from gcc
/// on the same source: prioritized constructors run first in ascending
/// priority, then the unprioritized ones in declaration order; destructors run
/// the other way, unprioritized first.
///
/// Every function here is `static` and none is ever called, so this also
/// covers them surviving dead-function elimination -- `compile_and_run`
/// includes an optimized configuration, where an unreferenced static function
/// is otherwise dropped.
#[test]
fn codegen_constructor_and_destructor_run() {
    let src = r#"
extern void _exit(int);

static int order[8];
static int n;
static void note(int v) { if (n < 8) order[n++] = v; }

__attribute__((constructor))      static void c_plain_a(void) { note(1); }
__attribute__((constructor))      static void c_plain_b(void) { note(2); }
__attribute__((constructor(101))) static void c_p101(void)    { note(101); }
__attribute__((constructor(102))) static void c_p102(void)    { note(102); }
__attribute__((__constructor__))  static void c_us(void)      { note(3); }
static void c_proto(void) __attribute__((constructor));
static void c_proto(void) { note(4); }

__attribute__((destructor))      static void d_plain(void) { note(50); }
__attribute__((destructor(102))) static void d_p102(void)  { note(51); }

static int status = 9;

static int check_ctors(void)
{
    if (n != 6) return 1;
    if (order[0] != 101) return 2;
    if (order[1] != 102) return 3;
    if (order[2] != 1) return 4;
    if (order[3] != 2) return 5;
    if (order[4] != 3) return 6;
    if (order[5] != 4) return 7;
    return 0;
}

/* Plain destructors run first, so this prioritized one goes last and gets to
   check what the others recorded. */
__attribute__((destructor(101))) static void d_last(void)
{
    if (status != 0) _exit(status);
    if (n != 8) _exit(20);
    if (order[6] != 50) _exit(21);
    if (order[7] != 51) _exit(22);
    _exit(0);
}

int main(void)
{
    status = check_ctors();
    /* 9 says the destructors never ran; d_last replaces it with the verdict */
    return status == 0 ? 9 : status;
}
"#;
    assert_eq!(compile_and_run("ctor_dtor_run", src, &[]), 0);
}

// ============================================================================
// GCC asm labels on declarations: __asm__("name")
// ============================================================================

/// An asm label renames the symbol a declaration refers to.
///
/// `extern int myfn(int) __asm__("realfn");` means: the source calls it `myfn`,
/// but every reference emits the symbol `realfn`. c17 used to parse the label
/// and throw it away, so it emitted `call myfn@PLT` where gcc emits
/// `call realfn@PLT` -- a wrong-symbol bug that links only by accident, when
/// both names happen to exist.
///
/// Measured against gcc. One gcc behaviour is deliberately *not* asserted here:
/// gcc folds `&alias == &real` to 0 at compile time even though a write through
/// one is visible through the other. That is a constant-folding artifact of
/// keeping two declarations distinct, not a property worth matching.
#[test]
fn codegen_asm_label_renames_the_symbol() {
    let src = r#"
#include <stdio.h>

/* An asm label is the assembler name itself, so naming a C object with one
   means spelling whatever prefix the target puts on a C identifier. This is
   glibc's `__ASMNAME`, and the reason __USER_LABEL_PREFIX__ exists: it is
   empty on ELF and "_" on Mach-O. */
#define XSTR(s) #s
#define STR(s) XSTR(s)
#define ASMNAME(cname) __asm__(STR(__USER_LABEL_PREFIX__) cname)

/* A call through an asm label reaches the labelled function. */
int realfn(int x) { return x * 3; }
extern int myfn(int) ASMNAME("realfn");

/* An asm label on a definition renames the emitted label, so a second
   declaration carrying the same label reaches that definition. */
int defrenamed(int x) ASMNAME("real_def");
int defrenamed(int x) { return x + 100; }
extern int reach_def(int) ASMNAME("real_def");

/* An asm label on a variable, and on a variable definition. */
int real_var = 42;
extern int my_var ASMNAME("real_var");
int def_var ASMNAME("real_def_var") = 7;
extern int reach_def_var ASMNAME("real_def_var");

int main(void)
{
    if (myfn(7) != 21) return 1;
    if (reach_def(1) != 101) return 2;
    if (my_var != 42) return 3;
    if (reach_def_var != 7) return 4;

    /* The alias and the real object are the same storage. */
    my_var = 99;
    if (real_var != 99) return 5;
    reach_def_var = 55;
    if (def_var != 55) return 6;

    /* A function pointer taken through the alias is the real function. */
    int (*fp)(int) = myfn;
    if (fp(5) != 15) return 7;
    if (fp != realfn) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("asm_label_rename", src, &[]), 0);
}

/// An asm label on a block-scope declaration with linkage renames it too, and
/// a block-scope redeclaration keeps the label an earlier declaration gave the
/// name. The block binder never settled either: the first emitted a reference
/// to `x`, which does not exist, and the second called `f` instead of
/// `impl_f`.
#[test]
fn codegen_asm_label_on_block_scope_declarations() {
    let src = r#"
#define XSTR(s) #s
#define STR(s) XSTR(s)
#define ASMNAME(cname) __asm__(STR(__USER_LABEL_PREFIX__) cname)

int y = 42;
int impl_f(void) { return 7; }
int f(void) ASMNAME("impl_f");

int main(void)
{
    extern int x ASMNAME("y");
    if (x != 42) return 1;
    int f(void);
    if (f() != 7) return 2;
    return 0;
}
"#;
    compile_and_run_everywhere("asm_label_block_scope", src);

    // The same through a second translation unit, where nothing but the
    // label can connect the name to the object.
    let unit_a = "int y = 42;\n";
    let unit_b = r#"
#define XSTR(s) #s
#define STR(s) XSTR(s)
int main(void)
{
    extern int x __asm__(STR(__USER_LABEL_PREFIX__) "y");
    return x == 42 ? 0 : 1;
}
"#;
    assert_eq!(
        compile_and_run_two_units("asm_label_block_scope_2tu", unit_a, unit_b, &[]),
        0
    );
}

/// `weak`, `visibility`, `section` and `used` were parsed and thrown away
/// while `__has_attribute` answered 1 for each — so a program could ask, be
/// told yes, and get none of the behaviour. `cc/ATTR.md` claimed every
/// attribute whose absence a program could observe was implemented, and these
/// four are exactly the counterexample.
///
/// Checked in the object file rather than the assembly, because the binding
/// and visibility a linker sees is the thing that matters. Expectations came
/// from `readelf` on gcc's output for the same source.
#[test]
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
fn codegen_symbol_attributes_reach_the_object_file() {
    use std::process::Command;

    let src = r#"
__attribute__((weak)) int weak_var = 1;
__attribute__((visibility("hidden"))) int hidden_var = 2;
__attribute__((section(".mysec"))) int placed_var = 3;
/* Zero-initialized: must go to the named section rather than .comm, which
   would let the linker place it anywhere. */
__attribute__((section(".myzero"))) int placed_zero;
int plain_var = 4;

__attribute__((weak)) int weak_fn(void) { return 1; }
__attribute__((visibility("hidden"))) int hidden_fn(void) { return 2; }
/* "default" is the absence of a directive, not a `.default` pseudo-op --
   emitting one fails the assembler, and CPython puts this on every public
   function. "protected" is a real directive and must still be emitted. */
__attribute__((visibility("default"))) int default_fn(void) { return 4; }
__attribute__((visibility("protected"))) int protected_fn(void) { return 5; }
/* A function's section needs the executable flag; with the data flags the
   program segfaults on the first call. */
__attribute__((section(".mytext"))) int placed_fn(void) { return 3; }

int main(void) { return weak_fn() + hidden_fn() + placed_fn() + default_fn() + protected_fn() - 15; }
"#;

    let dir = plib::tmp::Builder::new()
        .prefix("c17_symattrs_")
        .tempdir()
        .expect("tempdir");
    let c = dir.path().join("t.c");
    let obj = dir.path().join("t.o");
    std::fs::write(&c, src).expect("write source");

    let out = crate::common::run_c17(&["-c", c.to_str().unwrap(), "-o", obj.to_str().unwrap()]);
    assert!(out.success, "compile failed: {}", out.stderr);

    let syms = Command::new("readelf")
        .args(["-sW", obj.to_str().unwrap()])
        .output()
        .expect("readelf -s");
    let syms = String::from_utf8_lossy(&syms.stdout);
    let line = |name: &str| -> String {
        syms.lines()
            .find(|l| l.split_whitespace().last() == Some(name))
            .unwrap_or_else(|| panic!("no symbol {name} in:\n{syms}"))
            .to_string()
    };

    assert!(line("weak_var").contains("WEAK"), "{}", line("weak_var"));
    assert!(line("weak_fn").contains("WEAK"), "{}", line("weak_fn"));
    assert!(
        line("hidden_var").contains("HIDDEN"),
        "{}",
        line("hidden_var")
    );
    assert!(
        line("hidden_fn").contains("HIDDEN"),
        "{}",
        line("hidden_fn")
    );
    // The controls: an ordinary symbol keeps global binding and default
    // visibility, so the directives are not being emitted for everything.
    assert!(
        line("plain_var").contains("GLOBAL") && line("plain_var").contains("DEFAULT"),
        "{}",
        line("plain_var")
    );
    assert!(
        line("default_fn").contains("DEFAULT"),
        "{}",
        line("default_fn")
    );
    assert!(
        line("protected_fn").contains("PROTECTED"),
        "{}",
        line("protected_fn")
    );

    let secs = Command::new("readelf")
        .args(["-SW", obj.to_str().unwrap()])
        .output()
        .expect("readelf -S");
    let secs = String::from_utf8_lossy(&secs.stdout);
    let sec = |name: &str| -> String {
        secs.lines()
            .find(|l| l.contains(name))
            .unwrap_or_else(|| panic!("no section {name} in:\n{secs}"))
            .to_string()
    };
    // Data sections are allocated and writable; a code section is allocated
    // and executable.
    assert!(sec(".mysec").contains(" WA "), "{}", sec(".mysec"));
    assert!(sec(".myzero").contains(" WA "), "{}", sec(".myzero"));
    assert!(sec(".mytext").contains(" AX "), "{}", sec(".mytext"));
}

/// Seven defects found by review of the symbol-attribute and overflow-builtin
/// work, each reproduced against `gcc -std=c17` before being fixed.
///
/// The attribute cases are grouped because they share a shape: an attribute
/// that reaches the *parser* but not the object file, or reaches an object it
/// was never written on.
#[test]
fn codegen_symbol_attributes_do_not_leak_or_vanish() {
    // A function definition consumed its attributes through the function-attribute
    // path and left the symbol ones pending, so the next declaration claimed
    // them. The conflicting "ax"/"aw" section flags made the assembler reject
    // the file outright, so this is a build failure rather than bad linkage.
    //
    // ELF-only: Mach-O section names are `SEGMENT,section` pairs, so the
    // source itself would not assemble there.
    #[cfg(target_os = "linux")]
    {
        let leak = r#"
__attribute__((section(".mytext"))) int placed_fn(void) { return 3; }
int plain_var = 4;
__attribute__((weak)) int weak_fn(void) { return 1; }
int plain2 = 5;
int main(void) { return plain_var + plain2 + placed_fn() + weak_fn() - 13; }
"#;
        assert_eq!(
            compile_and_run("codegen_symbol_attrs_no_leak", leak, &[]),
            0
        );
    }

    // The same leak without a section, so it runs everywhere: `weak` on the
    // definition must not follow on to `plain2`.
    let leak_weak = r#"
__attribute__((weak)) int weak_fn(void) { return 1; }
int plain2 = 5;
int main(void) { return plain2 + weak_fn() - 6; }
"#;
    assert_eq!(
        compile_and_run("codegen_symbol_attrs_no_leak_weak", leak_weak, &[]),
        0
    );

    // `weak` on a declaration with no definition. Every spelling and position,
    // since only the trailing-attribute-on-a-function-declarator one was
    // broken.
    #[cfg(target_os = "linux")]
    let declared = r#"
extern int missing_a(void) __attribute__((weak));
__attribute__((weak)) extern int missing_b(void);
int missing_c(void) __attribute__((weak));
extern int missing_var __attribute__((weak));
int main(void) {
    if (missing_a) return 1;
    if (missing_b) return 2;
    if (missing_c) return 3;
    if (&missing_var) return 4;
    return 0;
}
"#;

    // What c17 emits for `declared` is checked in process, on every target:
    // `test_asm::codegen_asm_attributes::codegen_symbol_attributes_weak_declaration_directives`.

    // Whether an unresolved weak symbol *links* is the platform linker's
    // policy rather than the compiler's, so the running half is checked where
    // that policy is known.
    #[cfg(target_os = "linux")]
    assert_eq!(
        compile_and_run("codegen_symbol_attrs_weak_declaration", declared, &[]),
        0
    );
}

/// A program declaring a hidden function and object it never uses, and never
/// defines, links and runs -- perl's `Perl_do_exec` shape. A hidden one it does
/// use, defined in the other unit, still resolves.
#[test]
fn codegen_unreferenced_hidden_declaration_links() {
    let unit_a = r#"
extern int never_defined(void) __attribute__((visibility("hidden")));
extern int never_defined_var __attribute__((visibility("hidden")));
extern int helper(void) __attribute__((visibility("hidden")));
int main(void) { return helper() - 7; }
"#;
    let unit_b = r#"
__attribute__((visibility("hidden"))) int helper(void) { return 7; }
"#;
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run_two_units(
                "codegen_unref_hidden_decl",
                unit_a,
                unit_b,
                &[opt.to_string()]
            ),
            0,
            "{opt}"
        );
    }
}

/// The same declaration shape has to keep working as a program, not just as
/// assembly -- including the attributes that carry no symbol directive.
#[test]
fn codegen_attributes_on_later_declarators_compile_and_run() {
    let code = r#"
#include <stdlib.h>
int a1 = 1, a2 __attribute__((unused)) = 2, a3 = 3;
int b1 = 4, b2 __attribute__((aligned(64))) = 5;
static int c1 = 6, c2 __attribute__((used)) = 7;
extern int d1, d2 __attribute__((weak));
int d1 = 8, d2 = 9;

int main(void) {
    if (a1 + a2 + a3 != 6) return 1;
    if (b1 + b2 != 9) return 2;
    if (c1 + c2 != 13) return 3;
    if (d1 + d2 != 17) return 4;
    if ((unsigned long)&b2 % 64) return 5;
    /* The first declarator must not have taken b2's alignment. */
    {
        int local_x = 1, local_y __attribute__((unused)) = 2;
        if (local_x + local_y != 3) return 6;
    }
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_attrs_on_later_declarators", code, &[]),
        0
    );
}

/// `_Alignas`, and an attribute in the specifier position, belong to the
/// whole declaration and reach every declarator. An attribute written after
/// one declarator belongs to that declarator alone.
///
/// Both spellings shared one `pending_alignas` slot, which was cleared only
/// at the semicolon, so every declarator following an attributed one
/// inherited its alignment -- `int s __attribute__((aligned(64))), t;` gave
/// `t` 64 bytes of alignment that gcc does not.
#[test]
fn codegen_attribute_alignment_scope_follows_where_it_is_written() {
    let code = r#"
/* Post-declarator: names that declarator only. */
int p = 1, q __attribute__((aligned(64))) = 2, r = 3;
int s __attribute__((aligned(64))) = 4, t = 5;

/* Specifier position and _Alignas: name the whole declaration. */
int __attribute__((aligned(64))) u = 6, v = 7;
_Alignas(64) int w = 8, x = 9;

#define ALIGNED64(o) (((unsigned long)&(o) % 64) == 0)

int main(void) {
    if (!ALIGNED64(q)) return 1;
    if (!ALIGNED64(s)) return 2;
    /* The declarators after an attributed one keep natural alignment. */
    if (ALIGNED64(r)) return 3;
    if (ALIGNED64(t)) return 4;

    /* Declaration-wide spellings reach both names. */
    if (!ALIGNED64(u) || !ALIGNED64(v)) return 5;
    if (!ALIGNED64(w) || !ALIGNED64(x)) return 6;

    /* Values are undisturbed by any of it. */
    if (p + q + r + s + t != 15) return 7;
    if (u + v + w + x != 30) return 8;

    /* Same rules at block scope. */
    {
        int bp = 1, bq __attribute__((aligned(64))) = 2, br = 3;
        if (!ALIGNED64(bq)) return 9;
        if (ALIGNED64(br)) return 10;
        if (bp + bq + br != 6) return 11;
    }
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_attr_alignment_scope", code, &[]),
        0
    );
}

/// An over-aligned frame's base register is not one the inline asm claims.
///
/// `FrameBase::Aligned` withholds its register from `allocatable_regs`, but
/// inline asm bypasses allocation on both counts: a constraint letter pins an
/// operand to a fixed register outright, and a clobber only reaches the
/// constraint points, which spill *pseudos* -- and the frame base is not a
/// pseudo. The prologue writes it once and every local is addressed from it
/// for the rest of the body, so an `asm` naming it invalidated all of them at
/// a stroke.
///
/// Hard-coding `%rbx` made both spellings below segfault on code gcc compiles
/// without complaint. Asserted behaviourally rather than by naming the
/// register the compiler ought to pick instead -- which register is free is
/// exactly what the fix computes.
#[test]
#[cfg(target_arch = "x86_64")]
fn codegen_over_aligned_frame_base_avoids_asm_registers() {
    let code = r#"
int main(void) {
    __attribute__((aligned(32))) int arr[8];
    for (int i = 0; i < 8; i++) arr[i] = i * i;

    /* An explicit clobber of the register the base used to be hard-coded to. */
    unsigned long a;
    __asm__ volatile("movq $4660, %%rbx\n\tmovq %%rbx, %0" : "=r"(a) : : "rbx");

    /* And the other spelling: a "b" constraint pins the operand to %rbx
       without any clobber list at all. */
    unsigned long b;
    __asm__ volatile("movq %1, %0" : "=r"(b) : "b"(22136UL));

    /* The locals must have survived both. */
    int sum = 0;
    for (int i = 0; i < 8; i++) sum += arr[i];
    if (sum != 140) return 1;
    if (a != 4660) return 2;
    if (b != 22136) return 3;
    if (((unsigned long)arr & 31) != 0) return 4;
    return 0;
}
"#;
    assert_eq!(compile_and_run("frame_base_vs_asm", code, &[]), 0);
    assert_eq!(
        crate::common::compile_and_run_optimized("frame_base_vs_asm_opt", code),
        0
    );
}

/// The aarch64 twin of `codegen_over_aligned_frame_base_avoids_asm_registers`.
///
/// `FrameBase::Aligned` hard-coded x19 there for the same reason x86-64
/// hard-coded `%rbx`, and with the same consequence: the prologue writes the
/// base once and every local is addressed from it, so an `asm` clobbering it
/// invalidated all of them. gcc compiles this without complaint.
///
/// Separate from the x86-64 test rather than one test with a `cfg`-selected
/// template, because the two need different asm and aarch64 has no constraint
/// letter that pins a general register -- only the clobber spelling applies.
#[test]
#[cfg(target_arch = "aarch64")]
fn codegen_over_aligned_frame_base_avoids_asm_registers_aarch64() {
    let code = r#"
int main(void) {
    __attribute__((aligned(32))) int arr[8];
    for (int i = 0; i < 8; i++) arr[i] = i * i;

    unsigned long a;
    __asm__ volatile("mov x19, #4660\n\tmov %0, x19" : "=r"(a) : : "x19");

    int sum = 0;
    for (int i = 0; i < 8; i++) sum += arr[i];
    if (sum != 140) return 1;
    if (a != 4660) return 2;
    if (((unsigned long)arr & 31) != 0) return 3;
    return 0;
}
"#;
    assert_eq!(compile_and_run("frame_base_vs_asm_a64", code, &[]), 0);
    assert_eq!(
        crate::common::compile_and_run_optimized("frame_base_vs_asm_a64_opt", code),
        0
    );
}

/// A library function renamed with `__asm("name")` is called by that name
/// from every spelling that reaches it: the plain call, the `__builtin_`
/// form, and a copy or fill the compiler lowers to it. `__builtin_memcpy` and
/// `__builtin_memset` became the `Memcpy`/`Memset` opcodes, which both
/// backends emitted as calls to the literal `memcpy`/`memset`, bypassing the
/// program's rename. gcc.c-torture's `builtins/memops-asm`.
#[test]
fn codegen_asm_renamed_library_function_is_honoured() {
    let src = r#"
typedef __SIZE_TYPE__ size_t;
/* The copies below are byte-wise through volatile, or an optimizer turns the
   loop back into a memcpy call -- which the rename makes a call to itself. */
/* An asm label is the assembler name itself: spell the target's C prefix,
   empty on ELF and "_" on Mach-O, as the torture test does. */
#define XSTR(s) #s
#define STR(s) XSTR(s)
#define ASMNAME(cname) __asm(STR(__USER_LABEL_PREFIX__) cname)
extern void *memcpy(void *, const void *, size_t) ASMNAME("my_memcpy");
extern void *memset(void *, int, size_t) ASMNAME("my_memset");
extern void *memmove(void *, const void *, size_t) ASMNAME("my_memmove");

int calls;

__attribute__((used)) void *my_memcpy(void *d, const void *s, size_t n)
{
    volatile char *dp = d; const volatile char *sp = s;
    calls++;
    while (n--) *dp++ = *sp++;
    return d;
}
__attribute__((used)) void *my_memset(void *d, int c, size_t n)
{
    volatile char *dp = d;
    calls++;
    while (n--) *dp++ = c;
    return d;
}
__attribute__((used)) void *my_memmove(void *d, const void *s, size_t n)
{
    volatile char tmp[256]; volatile char *dp = d; const volatile char *sp = s;
    calls++;
    for (size_t i = 0; i < n; i++) tmp[i] = sp[i];
    for (size_t i = 0; i < n; i++) dp[i] = tmp[i];
    return d;
}

char x[64] = "foobar", y[64];
volatile int n = 6;

int main(void)
{
    if (__builtin_memcpy(y, x, n) != y || y[5] != 'r') return 1;
    if (__builtin_memset(y, 'X', n) != y || y[0] != 'X') return 2;
    if (__builtin_memmove(y + 1, y, n) != y + 1 || y[6] != 'X') return 3;
    if (memcpy(y, x, n) != y || memset(y, 0, n) != y) return 4;
    if (calls != 5) return 10 + calls;
    return 0;
}
"#;
    compile_and_run_everywhere("asm_renamed_libfn", src);
}

/// `__attribute__((alias("target")))` makes a second symbol for the storage or
/// code of a definition in the same unit. c17 ignored it with a warning, so
/// every use of the alias was an undefined reference at link time
/// (gcc.c-torture `alias-2`/`-3`/`-4`). Covers objects, an array and a struct,
/// functions, a static target reached only through its alias, a static alias,
/// a weak alias, an alias of an alias, and -- the part an optimizer can get
/// wrong -- a store through one name read back through the other.
// Mach-O has no symbol aliases, so c17 rejects `alias` on a Darwin host
// (`diagnostics_alias_attribute_unsupported_on_darwin` covers that side).
#[cfg(not(target_os = "macos"))]
#[test]
fn codegen_alias_attribute() {
    let src = r#"
int a[10] = {0};
extern int b[10] __attribute__((alias("a")));
static int s = 5;
extern int t __attribute__((alias("s")));
static int u __attribute__((alias("s")));
int f(void) { return 11; }
int g(void) __attribute__((alias("f")));
int w(void) __attribute__((weak, alias("f")));
int chained(void) __attribute__((alias("g")));
static int sf(void) { return 22; }
int h(void) __attribute__((alias("sf"), visibility("hidden")));
static int only_by_alias(void) { return 33; }
int public_name(void) __attribute__((alias("only_by_alias")));
static int lonely = 7;
extern int lonely_alias __attribute__((alias("lonely")));
struct pt { int x, y; } origin = {1, 2};
extern struct pt origin2 __attribute__((alias("origin")));
int off;

__attribute__((noinline)) static void bump(void) { t++; }

int main(void)
{
    b[off] = 1;
    a[off] = 2;
    if (b[off] != 2)
        return 1;
    if (&b[3] != &a[3])
        return 2;
    s = 0;
    bump();
    if (s != 1)
        return 3;
    if (&u != &s)
        return 4;
    if (g() != 11 || w() != 11 || chained() != 11)
        return 5;
    if (h() != 22)
        return 6;
    if (public_name() != 33)
        return 7;
    if (lonely_alias != 7)
        return 8;
    origin.y = 5;
    if (origin2.y != 5 || origin2.x != 1)
        return 9;
    int (*pg)(void) = g, (*pf)(void) = f;
    if (pg != pf)
        return 10;
    return 0;
}
"#;
    compile_and_run_everywhere("alias_attribute", src);
}

/// An alias is a symbol other translation units link against like any other:
/// a write through the alias from one unit is a write to the target's storage,
/// a call through a function alias reaches the target, and a *weak* alias
/// gives way to a strong definition elsewhere while the target keeps its own
/// name.
// Mach-O has no symbol aliases, so c17 rejects `alias` on a Darwin host
// (`diagnostics_alias_attribute_unsupported_on_darwin` covers that side).
#[cfg(not(target_os = "macos"))]
#[test]
fn codegen_alias_attribute_across_units() {
    let unit_a = r#"
int store[4] = {1, 2, 3, 4};
extern int view[4] __attribute__((alias("store")));
int impl(int x) { return x * 2; }
int api(int) __attribute__((alias("impl")));
int fallback(void) { return 1; }
int hook(void) __attribute__((weak, alias("fallback")));
int unhooked(void) __attribute__((weak, alias("fallback")));
int read_store(int i) { return store[i]; }
"#;
    let unit_b = r#"
extern int view[4];
int api(int);
int fallback(void);
int unhooked(void);
int read_store(int);
int hook(void) { return 2; }

int main(void)
{
    view[2] = 30;
    if (read_store(2) != 30)
        return 1;
    if (api(21) != 42)
        return 2;
    if (hook() != 2)
        return 3;
    if (fallback() != 1 || unhooked() != 1)
        return 4;
    return 0;
}
"#;
    for opt in [&[][..], &["-O2".to_string()][..]] {
        assert_eq!(
            compile_and_run_two_units("alias_units", unit_a, unit_b, opt),
            0,
            "alias across units at {opt:?}"
        );
    }
}

/// GNU attributes inside a type-name -- in a cast, `sizeof`, `_Alignof`,
/// `typeof` and a compound literal -- were rejected. `aligned` aligns the
/// type named, `mode` and `vector_size` replace it, and none of them reach an
/// enclosing declaration. Alongside: a tagged struct defined in a type-name
/// declares its tag, `aligned` on a tag reference aligns the declaration, and
/// an enclosing `_Alignas` no longer lands on a struct's first member. Every
/// answer is gcc's, on both targets.
#[test]
fn codegen_type_name_attributes() {
    let src = r#"
struct S { int a; char b; };
#define A(x) __attribute__((x))

/* A type-name's attribute is its own: neither `c` nor `x` may pick it up. */
char c = sizeof(int * A(aligned(64)));
_Alignas(16) struct { char a; char b; } x;
A(aligned(32)) struct Q { char a; char b; } y;
struct S A(aligned(32)) z;
typedef struct S A(aligned(64)) T;

int main(void) {
    struct S s = {1, 2};
    void *p = &s;
    if (sizeof(int A(packed)) != 4) return 1;
    if (sizeof(A(packed) int) != 4) return 2;
    if (sizeof(long A(aligned(16))) != 8) return 3;
    if (_Alignof(int A(aligned(8))) != 8) return 4;
    if (_Alignof(A(aligned(8)) int) != 8) return 5;
    if (_Alignof(int A(aligned(2))) != 2) return 6;
    if (_Alignof(char A(aligned)) != 16) return 7;
    if (_Alignof(struct S A(aligned(32))) != 32) return 8;
    if (_Alignof(const A(aligned(16)) long) != 16) return 9;
    if (_Alignof(int A(aligned(16)) *) != 16) return 10;
    if (_Alignof(int * A(aligned(16))) != 16) return 11;
    if (sizeof(int A(aligned(16))[3]) != 12) return 12;
    if ((long A(aligned(16)))0 + 5 != 5) return 13;
    if (((struct S A(may_alias) *)p)->a != 1) return 14;
    if (((A(may_alias) struct S *)p)->b != 2) return 15;
    if ((int A(unused)){7} != 7) return 16;
    if (_Alignof(__typeof__(int A(aligned(16)))) != 16) return 17;
    if (sizeof(int A(mode(DI))) != 8) return 18;
    if (sizeof(int A(vector_size(16))) != 16) return 19;
    if (_Alignof(c) != 1) return 20;
    if (sizeof(x) != 2 || _Alignof(x) != 16) return 21;
    if (sizeof(y) != 2 || _Alignof(y) != 32) return 22;
    if (_Alignof(z) != 32 || _Alignof(T) != 64 || _Alignof(struct S) != 4) return 23;
    /* A tagged definition inside a type-name declares the tag. */
    int n = sizeof(struct U { int a, b; });
    struct U u = {3, 4};
    if (n != 8) return 24;
    if (((struct U *)&u)->b != 4) return 25;
    return 0;
}
"#;
    compile_and_run_everywhere("type_name_attrs", src);
}
