//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-aux-info FILE`: a record of every function the unit declares or defines.
//
// libselinux's exception.sh pipes its public headers through `${CC:-gcc} -x c
// -c - -aux-info temp.aux`, falling back to `gcc`, and builds the Python
// binding's SWIG exception list from the result with
// `awk '/<stdin>.*extern int/ { print $6 }'`. Without the file the Debian
// build of libselinux fails, so the record has to come out in gcc's shape.
//

use plib::testing::run_test_base;

const SRC: &str = "\
struct ctx;
typedef void (*logger)(const char *, ...);
extern int is_enabled(void);
int getcon(char **con);
void freecon(char *);
char *subst(const char *);
int set_logger(logger fn);
unsigned int flags(void);
int (*callback(int which))(struct ctx *);
static int helper(int x) { return x; }
int defined_here(int a, long b) { return helper(a) + (int)b; }
int old_style();
";

/// Compile `SRC` from standard input with `-aux-info`, as libselinux does,
/// and answer the record.
fn aux_info_from_stdin() -> String {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_aux_info_")
        .tempdir()
        .expect("tempdir");
    let aux = dir.path().join("temp.aux");
    let obj = dir.path().join("temp.o");
    let args: Vec<String> = [
        "-x",
        "c",
        "-c",
        "-o",
        obj.to_str().unwrap(),
        "-",
        "-aux-info",
        aux.to_str().unwrap(),
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();
    let out = run_test_base("c17", &args, SRC.as_bytes());
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(obj.exists(), "the object is still written");
    std::fs::read_to_string(&aux).expect("aux-info file written")
}

/// The record line for line as gcc 14 writes it, but for one difference:
/// c17's types do not remember the typedef a declaration named, so `logger`
/// is spelled as the type it stands for.
#[test]
fn driver_aux_info_records_each_function() {
    let text = aux_info_from_stdin();
    let lines: Vec<&str> = text.lines().filter(|l| l.contains("<stdin>")).collect();
    let expected = [
        "/* <stdin>:3:NC */ extern int is_enabled (void);",
        "/* <stdin>:4:NC */ extern int getcon (char **);",
        "/* <stdin>:5:NC */ extern void freecon (char *);",
        "/* <stdin>:6:NC */ extern char *subst (const char *);",
        "/* <stdin>:7:NC */ extern int set_logger (void (*) (const char *, ...));",
        "/* <stdin>:8:NC */ extern unsigned int flags (void);",
        "/* <stdin>:9:NC */ extern int (*callback (int)) (struct ctx *);",
        "/* <stdin>:10:NF */ static int helper (int x); /* (x) int x; */",
        "/* <stdin>:11:NF */ extern int defined_here (int a, long int b); /* (a, b) int a; long int b; */",
        "/* <stdin>:12:OC */ extern int old_style (/* ??? */);",
    ];
    assert_eq!(lines, expected, "{text}");
}

/// What libselinux's awk takes from the record: the name of every function
/// declared in the piped text that returns `int`.
#[test]
fn driver_aux_info_feeds_libselinux_awk() {
    let text = aux_info_from_stdin();
    let names: Vec<&str> = text
        .lines()
        .filter(|l| l.contains("<stdin>") && l.contains("extern int"))
        .filter_map(|l| l.split_whitespace().nth(5))
        .collect();
    assert_eq!(
        names,
        [
            "is_enabled",
            "getcon",
            "set_logger",
            "(*callback",
            "defined_here",
            "old_style"
        ]
    );
}
