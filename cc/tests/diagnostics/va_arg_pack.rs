//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A `__builtin_va_arg_pack` forwarder that would need an out-of-line copy.
//
// The builtins name the *caller's* variadic arguments, so a forwarder exists
// only where it is inlined into a call. GCC rejects every program that would
// need it as an object of its own -- its address taken and still referenced
// once optimization is done, or an external definition -- with
// "invalid use of '__builtin_va_arg_pack ()'". Without the diagnostic the
// program failed at link time with an undefined reference to the forwarder.
//

use crate::common::{compile_rejected_with, create_c_file, run_c17};

const OPTS: [&str; 2] = ["-O0", "-O2"];

/// Require `src` to be rejected at `opt` with the invalid-use diagnostic for
/// `builtin`, reported on line `line`.
fn expect_invalid_use(name: &str, src: &str, opt: &str, builtin: &str, line: u32) {
    let stderr = compile_rejected_with(&format!("{name}{opt}"), src, &[opt]);
    let wanted = format!("invalid use of '{builtin} ()'");
    assert!(
        stderr.contains(&wanted),
        "{name} {opt}: expected {wanted:?}, got:\n{stderr}"
    );
    assert!(
        stderr.contains(&format!(":{line}:")),
        "{name} {opt}: expected the report on line {line}, got:\n{stderr}"
    );
}

/// Require `src` to compile at `opt` with no invalid-use diagnostic.
fn expect_accepted(name: &str, src: &str, opt: &str) {
    let c = create_c_file(&format!("{name}{opt}"), src);
    let path = c.path().to_string_lossy().to_string();
    let run = run_c17(&[opt, "-S", "-o", "/dev/null", &path]);
    assert!(run.success, "{name} {opt} should compile:\n{}", run.stderr);
    assert!(
        !run.stderr.contains("invalid use"),
        "{name} {opt}: unexpected diagnostic:\n{}",
        run.stderr
    );
}

const COUNT: &str = "static inline __attribute__((always_inline)) int count(const char *f, ...)\n\
                     { (void)f; return __builtin_va_arg_pack_len(); }\n";

const WRAP: &str = "extern int printf(const char *, ...);\n\
                    static inline __attribute__((always_inline)) int wrap(const char *f, ...)\n\
                    { return printf(f, __builtin_va_arg_pack()); }\n";

/// The address stored in a pointer and called through it. Reported where the
/// address is taken: that is what asks for the copy.
#[test]
fn va_arg_pack_len_forwarder_address_taken() {
    let src = format!(
        "{COUNT}int main(void) {{\n\
         int (*p)(const char *, ...) = count;\n\
         return p(\"x\", 1); }}\n"
    );
    for opt in OPTS {
        expect_invalid_use("pack_len_addr", &src, opt, "__builtin_va_arg_pack_len", 4);
    }
}

/// The forwarder passed as a function-pointer argument, for both builtins.
#[test]
fn va_arg_pack_forwarder_passed_as_argument() {
    let src = format!(
        "{WRAP}extern int use(int (*)(const char *, ...));\n\
         int main(void) {{\n\
         return use(wrap); }}\n"
    );
    for opt in OPTS {
        expect_invalid_use("pack_arg", &src, opt, "__builtin_va_arg_pack", 6);
    }
    let src = format!(
        "{COUNT}extern int use(int (*)(const char *, ...));\n\
         int main(void) {{\n\
         return use(count); }}\n"
    );
    for opt in OPTS {
        expect_invalid_use("pack_len_arg", &src, opt, "__builtin_va_arg_pack_len", 5);
    }
}

/// An address in a static initializer has no use-site position, so the report
/// falls back to the builtin, which is where gcc puts it.
#[test]
fn va_arg_pack_forwarder_in_initializer() {
    let src = format!(
        "{WRAP}int (*tbl[])(const char *, ...) = {{ wrap }};\n\
         int main(void) {{ return tbl[0](\"\\n\"); }}\n"
    );
    for opt in OPTS {
        expect_invalid_use("pack_init", &src, opt, "__builtin_va_arg_pack", 3);
    }
}

/// A forwarder that is an external definition must be emitted, so it cannot
/// forward anything, even though every call in this file is inlined.
#[test]
fn va_arg_pack_forwarder_external_definition() {
    let src = "__attribute__((always_inline)) inline int count(const char *f, ...)\n\
               { (void)f; return __builtin_va_arg_pack_len(); }\n\
               extern int count(const char *, ...);\n\
               int main(void) { return count(\"a\", 1, 2) == 2 ? 0 : 1; }\n";
    for opt in OPTS {
        expect_invalid_use("pack_extdef", src, opt, "__builtin_va_arg_pack_len", 2);
    }
}

/// An address the optimizer deletes needs no copy: gcc rejects this at -O0
/// and accepts it at -O2, and so does c17.
#[test]
fn va_arg_pack_forwarder_dead_address() {
    let src = format!(
        "{COUNT}int main(void) {{\n\
         int (*p)(const char *, ...) = count;\n\
         (void)p; return count(\"x\"); }}\n"
    );
    expect_invalid_use(
        "pack_dead_addr",
        &src,
        "-O0",
        "__builtin_va_arg_pack_len",
        4,
    );
    expect_accepted("pack_dead_addr", &src, "-O2");
}

/// What must still be accepted: an inline definition's address names the
/// external definition another translation unit provides, under both the GNU
/// and the C99 rules, and `sizeof` evaluates nothing.
#[test]
fn va_arg_pack_forwarder_address_without_copy_accepted() {
    let gnu = "extern int use(int (*)(const char *, ...));\n\
               extern inline __attribute__((always_inline, gnu_inline))\n\
               int count(const char *f, ...) { (void)f; return __builtin_va_arg_pack_len(); }\n\
               int main(void) { return use(count) + count(\"x\", 1); }\n";
    let c99 = "extern int use(int (*)(const char *, ...));\n\
               inline __attribute__((always_inline))\n\
               int count(const char *f, ...) { (void)f; return __builtin_va_arg_pack_len(); }\n\
               int main(void) { return use(count) + count(\"x\", 1); }\n";
    let unevaluated = format!("{COUNT}int main(void) {{ return sizeof(&count) == 0; }}\n");
    for opt in OPTS {
        expect_accepted("pack_gnu_inline_addr", gnu, opt);
        expect_accepted("pack_c99_inline_addr", c99, opt);
        expect_accepted("pack_sizeof_addr", &unevaluated, opt);
    }
}
