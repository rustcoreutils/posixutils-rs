//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Which inline definitions put a symbol in the object, whatever the function
// returns. A struct specifier names the tag's one shared type, so a storage
// class read off the return type was lost for a struct, a pointer to one or
// a typedef of one: a gnu_inline `extern inline` returning one was emitted,
// and two objects including glibc's `<sys/socket.h>` (`__cmsg_nxthdr`)
// failed to link.
//

use crate::common::{c17_object_with, compile_and_run_two_units};
use std::process::Command;

/// The `nm` type letter of `name`'s definition in `obj`, if it defines it.
/// A Mach-O symbol carries a leading underscore.
fn defined_as(obj: &str, name: &str) -> Option<char> {
    let out = Command::new("nm").arg(obj).output().expect("run nm");
    assert!(out.status.success(), "nm {obj} failed");
    let text = String::from_utf8_lossy(&out.stdout).into_owned();
    let underscored = format!("_{name}");
    text.lines().find_map(|line| {
        let fields: Vec<&str> = line.split_whitespace().collect();
        match fields.as_slice() {
            [_, kind, sym] if *sym == name || *sym == underscored => kind.chars().next(),
            _ => None,
        }
    })
}

const GNU: &str = "__attribute__((__gnu_inline__))";

const RETURNS: [&str; 5] = ["int", "int *", "struct S", "struct S *", "T"];

/// The symbol each definition leaves in the object, at -O0 and -O2, for
/// every return type: `extern inline` gnu_inline none, a plain gnu_inline
/// `inline` a global one, C17 `extern inline` a global one, C17 `inline`
/// none, `static inline` a local one. gcc 13 agrees on every row.
#[test]
fn inline_definition_symbols_ignore_the_return_type() {
    let gnu_extern = format!("extern inline {GNU}");
    let gnu_plain = format!("inline {GNU}");
    // (specifiers, the nm letter of the definition)
    let cases: [(&str, Option<char>); 5] = [
        (&gnu_extern, None),
        (&gnu_plain, Some('T')),
        ("extern inline", Some('T')),
        ("inline", None),
        ("static inline", Some('t')),
    ];
    let dir = plib::tmp::Builder::new()
        .prefix("c17_gnuinl_nm_")
        .tempdir()
        .expect("tempdir");
    for opt in ["-O0", "-O2"] {
        for (r, ret) in RETURNS.iter().enumerate() {
            for (c, (specs, want)) in cases.iter().enumerate() {
                let src = format!(
                    "struct S {{ int b; }};\ntypedef struct S T;\n\
                     {specs} {ret} f(void) {{ {ret} r = {{0}}; return r; }}\n\
                     {ret} (*volatile use)(void) = f;\n"
                );
                let name = format!("gnuinl_{}_{r}_{c}", &opt[1..]);
                let obj = c17_object_with(&name, &src, opt, &[], dir.path());
                assert_eq!(defined_as(&obj, "f"), *want, "{opt}:\n{src}");
            }
        }
    }
}

/// `-fgnu89-inline` gives every inline function the rule the attribute
/// selects: `extern inline` emits nothing, whatever it returns.
#[test]
fn gnu89_extern_inline_emits_nothing_for_any_return() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_gnu89inl_nm_")
        .tempdir()
        .expect("tempdir");
    for opt in ["-O0", "-O2"] {
        for (r, ret) in RETURNS.iter().enumerate() {
            let src = format!(
                "struct S {{ int b; }};\ntypedef struct S T;\n\
                 extern inline {ret} f(void) {{ {ret} r = {{0}}; return r; }}\n\
                 {ret} (*volatile use)(void) = f;\n"
            );
            let name = format!("gnu89inl_{}_{r}", &opt[1..]);
            let obj = c17_object_with(&name, &src, opt, &["-fgnu89-inline"], dir.path());
            assert_eq!(defined_as(&obj, "f"), None, "{opt}:\n{src}");
        }
    }
}

/// The real definition after a gnu_inline `extern inline` body is the
/// function: it is emitted, global, whatever it returns.
#[test]
fn real_definition_after_gnu_inline_body_is_emitted() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_gnuinl_real_")
        .tempdir()
        .expect("tempdir");
    for opt in ["-O0", "-O2"] {
        for (r, ret) in RETURNS.iter().enumerate() {
            let src = format!(
                "struct S {{ int b; }};\ntypedef struct S T;\n\
                 extern inline {GNU} {ret} f(void) {{ {ret} r = {{0}}; return r; }}\n\
                 {ret} f(void) {{ {ret} r = {{0}}; return r; }}\n"
            );
            let name = format!("gnuinl_real_{}_{r}", &opt[1..]);
            let obj = c17_object_with(&name, &src, opt, &[], dir.path());
            assert_eq!(defined_as(&obj, "f"), Some('T'), "{opt}:\n{src}");
        }
    }
}

/// Two units including one header whose gnu_inline `extern inline` function
/// returns a pointer to a struct link together, the one real definition in
/// the first: the header put a second, inline-only copy in each.
#[test]
fn gnu_inline_struct_pointer_header_links_in_two_units() {
    let header = format!(
        "struct S {{ int b; }};\n\
         extern struct S *next(struct S *p);\n\
         extern inline {GNU} struct S *next(struct S *p) {{ return p->b ? p + 1 : 0; }}\n"
    );
    let unit_a = format!(
        "{header}struct S *next(struct S *p) {{ return p->b ? p + 1 : 0; }}\n\
         int a(struct S *p) {{ return next(p) == p + 1; }}\n"
    );
    let unit_b = format!(
        "{header}int a(struct S *p);\n\
         int main(void) {{\n\
           struct S s[2] = {{{{1}}, {{0}}}};\n\
           if (!a(&s[0])) return 1;\n\
           if (next(&s[1]) != 0) return 2;\n\
           return 0;\n\
         }}\n"
    );
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run_two_units("gnuinl_two", &unit_a, &unit_b, &[opt.to_string()]),
            0,
            "{opt}"
        );
    }
}

/// glibc's own instance: `<sys/socket.h>` under `_GNU_SOURCE` defines
/// `__cmsg_nxthdr` gnu_inline `extern inline`, returning `struct cmsghdr *`.
/// Two units including it link, the real one coming from libc.
#[cfg(all(target_os = "linux", target_env = "gnu"))]
#[test]
fn sys_socket_header_links_in_two_units() {
    let walk = "#include <sys/socket.h>\n\
                int count(struct msghdr *m) {\n\
                  int n = 0;\n\
                  for (struct cmsghdr *c = CMSG_FIRSTHDR(m); c; c = CMSG_NXTHDR(m, c)) n++;\n\
                  return n;\n\
                }\n";
    let unit_a = walk.replace("count", "count_a");
    let unit_b = format!(
        "{}int count_a(struct msghdr *m);\n\
         int main(void) {{\n\
           union {{ char buf[CMSG_SPACE(sizeof(int)) * 2]; struct cmsghdr align; }} u = {{0}};\n\
           struct msghdr m = {{0}};\n\
           m.msg_control = u.buf;\n\
           m.msg_controllen = sizeof u.buf;\n\
           struct cmsghdr *c = CMSG_FIRSTHDR(&m);\n\
           c->cmsg_len = CMSG_LEN(sizeof(int));\n\
           c = CMSG_NXTHDR(&m, c);\n\
           c->cmsg_len = CMSG_LEN(sizeof(int));\n\
           return count_a(&m) == 2 && count(&m) == 2 ? 0 : 1;\n\
         }}\n",
        walk
    );
    for opt in ["-O0", "-O2"] {
        let flags = [opt.to_string(), "-D_GNU_SOURCE".to_string()];
        assert_eq!(
            compile_and_run_two_units("gnuinl_socket", &unit_a, &unit_b, &flags),
            0,
            "{opt}"
        );
    }
}
