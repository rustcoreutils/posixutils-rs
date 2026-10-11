//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The warning groups of the conversions "as if by assignment" (C17
// 6.5.16.1p1), checked against gcc 13: pointers whose targets differ only in
// signedness are `-Wpointer-sign`, off unless `-Wall` or `-Wpedantic` asks
// for it; other mismatched pointers are `-Wincompatible-pointer-types`; and
// each group answers to `-Wno-<name>` and `-Werror=<name>`.
//

use crate::test_compile::{compile_rejected_with, compile_warnings};

/// Every context that converts as if by assignment, with targets that differ
/// only in signedness -- the kernel's `atomic64_try_cmpxchg(s64 *)` handed a
/// `u64 *` among them -- and one genuinely mismatched pointer.
const SIGNS: &str = "\
void g(unsigned char *p);
void h(unsigned *p);
typedef long long s64; typedef unsigned long long u64;
int try_cmpxchg(s64 *old);
void f(long *p);
void t(void) {
  int i; char c[4]; int x; u64 o;
  g(c);
  h(&x);
  try_cmpxchg(&o);
  unsigned char *uc = c;
  char *cp; cp = uc;
  f(&i);
}
char *r(unsigned char *u) { return u; }
";

const SIGN_WARNINGS: [&str; 6] = [
    "8:5: pointer targets in passing argument 1 of 'g' differ in signedness [-Wpointer-sign]",
    "9:5: pointer targets in passing argument 1 of 'h' differ in signedness [-Wpointer-sign]",
    "10:15: pointer targets in passing argument 1 of 'try_cmpxchg' differ in signedness \
     [-Wpointer-sign]",
    "11:23: pointer targets in initialization of 'unsigned char *' from 'char *' differ in \
     signedness [-Wpointer-sign]",
    "12:16: pointer targets in assignment from 'unsigned char *' to 'char *' differ in \
     signedness [-Wpointer-sign]",
    "15:36: pointer targets in returning 'unsigned char *' from a function with return type \
     'char *' differ in signedness [-Wpointer-sign]",
];

/// The mismatched pointer's warning, without its tag.
const MISMATCH_MSG: &str = "passing argument 1 of 'f' from incompatible pointer type";
const MISMATCH: &str =
    "13:5: passing argument 1 of 'f' from incompatible pointer type [-Wincompatible-pointer-types]";

#[track_caller]
fn warnings(flags: &[&str]) -> Vec<String> {
    compile_warnings("pointer_sign", SIGNS, flags)
}

/// gcc gives no `-Wpointer-sign` warning by default, and one for each
/// conversion under `-Wall` or `-Wpedantic`.
#[test]
fn pointer_sign_is_enabled_by_wall_and_wpedantic() {
    assert_eq!(warnings(&[]), [MISMATCH]);
    for flags in [
        &["-Wall"][..],
        &["-Wpedantic"],
        &["-pedantic"],
        &["-Wpointer-sign"],
    ] {
        let mut expected: Vec<&str> = SIGN_WARNINGS.to_vec();
        expected.insert(5, MISMATCH);
        assert_eq!(warnings(flags), expected, "{flags:?}");
    }
}

/// An explicit `-W[no-]pointer-sign` outranks `-Wall` and `-Wpedantic` in
/// either order; among those two, the last one said wins.
#[test]
fn pointer_sign_folds_as_gcc_does() {
    for flags in [
        &["-Wall", "-Wno-pointer-sign"][..],
        &["-Wno-pointer-sign", "-Wall"],
        &["-Wno-pointer-sign", "-pedantic"],
        &["-Wall", "-Wno-all"],
        &["-Wall", "-Wno-pedantic"],
        &["-Werror=pointer-sign", "-Wno-pointer-sign"],
        &["-Wextra"],
    ] {
        assert_eq!(warnings(flags), [MISMATCH], "{flags:?}");
    }
    for flags in [
        &["-Wno-all", "-Wall"][..],
        &["-Wpointer-sign", "-Wno-all"],
        &["-Wno-pedantic", "-Wall"],
    ] {
        assert_eq!(warnings(flags).len(), 7, "{flags:?}");
    }
}

/// `-Werror=pointer-sign` turns the group on and makes it an error; under
/// `-Werror` it carries its own name, and `-Wno-error=pointer-sign` leaves it
/// a warning.
#[test]
fn pointer_sign_answers_to_werror() {
    let c = compile_rejected_with("pointer_sign_werror", SIGNS, &["-Werror=pointer-sign"]);
    assert!(c.contains(
        "pointer_sign_werror.c:8:5: error: pointer targets in passing argument 1 of 'g' \
         differ in signedness [-Werror=pointer-sign]\n"
    ));
    assert!(
        c.contains("c17: some warnings being treated as errors\n"),
        "{c}"
    );
    assert!(
        c.contains(&format!(
            "warning: {}",
            MISMATCH.split_once(": ").unwrap().1
        )),
        "{c}"
    );

    let c = compile_rejected_with(
        "pointer_sign_no_error",
        SIGNS,
        &["-Wall", "-Werror", "-Wno-error=pointer-sign"],
    );
    assert!(
        c.contains(&format!(
            "warning: {}\n",
            SIGN_WARNINGS[0].split_once(": ").unwrap().1
        )),
        "{c}"
    );
    assert!(
        c.contains(&format!(
            "error: {MISMATCH_MSG} [-Werror=incompatible-pointer-types]\n"
        )),
        "{c}"
    );
    assert!(
        c.contains("c17: all warnings being treated as errors\n"),
        "{c}"
    );
}

/// `-pedantic-errors` makes the signedness warnings errors, as the pedwarns
/// they are in gcc.
#[test]
fn pointer_sign_is_a_pedwarn() {
    let c = compile_rejected_with("pointer_sign_pe", SIGNS, &["-pedantic-errors"]);
    assert!(c.contains(
        "pointer_sign_pe.c:9:5: error: pointer targets in passing argument 1 of 'h' \
         differ in signedness [-Wpointer-sign]\n"
    ));
}

/// Only the signedness of the target counts: `enum`, `_Bool`, a different
/// width, a pointer one level down and an `_Atomic` target stay incompatible
/// pointers, and a dropped qualifier is reported as that alone.
#[test]
fn pointer_sign_is_only_signedness() {
    let src = "\
enum E { A, B };
void a(int *p); void b(unsigned *p); void c(long long *p); void d(unsigned char *p);
void e(signed char *p); void q(char (*p)[4]); void pp(unsigned char **p);
void t(void) {
  enum E e1; long l; const char cc[2]; char ch; _Atomic int ai; unsigned char ua[4]; char *cpp;
  a(&e1); c(&l); d(cc); e(&ch); b(&ai); q(&ua); pp(&cpp);
  _Bool *bp = &ch;
}
";
    let w = compile_warnings("pointer_sign_only", src, &["-Wall"]);
    let sign: Vec<&String> = w.iter().filter(|m| m.contains("signedness")).collect();
    assert_eq!(
        sign,
        [
            "6:27: pointer targets in passing argument 1 of 'e' differ in signedness \
          [-Wpointer-sign]"
        ],
        "{w:#?}"
    );
    assert!(
        w.iter()
            .any(|m| m.starts_with("6:20: ") && m.contains("discards 'const' qualifier")),
        "{w:#?}"
    );
    assert_eq!(
        w.iter()
            .filter(|m| m.contains("incompatible pointer type"))
            .count(),
        6,
        "{w:#?}"
    );
}

/// Each group of the assignment conversions answers to its own name, as
/// gcc's do: `-Wno-<name>` silences it and `-Werror=<name>` makes it an
/// error, leaving the other groups alone.
#[test]
fn assignment_conversion_groups() {
    let src = "\
const char *cs; char *ncs; long *lp;
void t(void) {
  int *p = 5;
  int i = p;
  ncs = cs;
  int *ip = lp;
}
";
    let cases = [
        ("int-conversion", "makes pointer from integer", 2),
        ("discarded-qualifiers", "discards 'const' qualifier", 1),
        ("incompatible-pointer-types", "incompatible pointer type", 1),
    ];
    let all = compile_warnings("assign_groups", src, &[]);
    assert_eq!(all.len(), 4, "{all:#?}");
    for (group, text, count) in cases {
        let off = compile_warnings("assign_groups_off", src, &[&format!("-Wno-{group}")]);
        assert_eq!(off.len(), 4 - count, "-Wno-{group}: {off:#?}");
        assert!(off.iter().all(|w| !w.contains(text)), "{off:#?}");

        let err = compile_rejected_with("assign_groups_err", src, &[&format!("-Werror={group}")]);
        assert!(
            err.contains(&format!("[-Werror={group}]\n")),
            "-Werror={group}:\n{err}"
        );
        assert_eq!(
            err.matches(&format!("[-Werror={group}]")).count(),
            count,
            "{err}"
        );
        assert_eq!(err.matches("warning: ").count(), 4 - count, "{err}");
    }
}

/// Pointers to arrays whose elements differ only in qualifiers: C2x makes
/// the arrays so qualified, and gcc 13 converts between them as between
/// differently qualified pointers -- `-pedantic` notes that C17 calls the
/// array types different, and dropping a qualifier is
/// `-Wdiscarded-array-qualifiers`, by default. `_Atomic` is no such
/// qualifier, and a pointer to such a pointer is incompatible as before.
const QUALIFIED_ARRAYS: &str = "\
void takes_v(volatile int (*p)[3]);
void takes(int (*p)[3]);
void f(int (*p)[3], const int (*c)[3], const volatile int (*cv)[3]) {
  volatile int (*v)[3] = p;
  const int (*c2)[3]; c2 = p;
  takes_v(p);
  int (*q)[3]; q = c;
  takes(cv);
  const int (*m)[2][3] = (int (*)[2][3])0 + 1;
  _Atomic int (*a)[3] = p;
  const int (**pp)[3] = &p;
  (void)v; (void)c2; (void)q; (void)m; (void)a; (void)pp;
}
const int (*g(int (*p)[3]))[3] { return p; }
int (*h(const int (*p)[3]))[3] { return p; }
";

#[test]
fn pointers_to_differently_qualified_arrays_convert_as_gcc() {
    const PEDANTIC: &str =
        "invalid use of pointers to arrays with different qualifiers in ISO C before C2X [-Wpedantic]";
    let by_default = [
        "7:18: assignment discards 'const' qualifier from pointer target type \
         [-Wdiscarded-array-qualifiers]",
        "8:9: passing argument 1 of 'takes' discards 'const volatile' qualifier from pointer \
         target type [-Wdiscarded-array-qualifiers]",
        "10:25: initialization of '_Atomic int (*)[3]' from incompatible pointer type \
         'int (*)[3]' [-Wincompatible-pointer-types]",
        "11:25: initialization of 'const int (**)[3]' from incompatible pointer type \
         'int (**)[3]' [-Wincompatible-pointer-types]",
        "15:41: return discards 'const' qualifier from pointer target type \
         [-Wdiscarded-array-qualifiers]",
    ];
    let quiet = compile_warnings("qual_arrays", QUALIFIED_ARRAYS, &[]);
    assert_eq!(quiet, by_default, "{quiet:#?}");

    let loud = compile_warnings("qual_arrays_pedantic", QUALIFIED_ARRAYS, &["-pedantic"]);
    let pedantic: Vec<&str> = loud
        .iter()
        .filter(|w| w.ends_with(PEDANTIC))
        .map(|w| w.split(": ").next().unwrap())
        .collect();
    assert_eq!(
        pedantic,
        ["4:26", "5:26", "6:11", "7:18", "8:9", "9:26", "14:41", "15:41"],
        "{loud:#?}"
    );
    assert_eq!(loud.len(), pedantic.len() + by_default.len(), "{loud:#?}");

    let err = compile_rejected_with(
        "qual_arrays_err",
        QUALIFIED_ARRAYS,
        &["-Werror=discarded-array-qualifiers"],
    );
    assert_eq!(
        err.matches("[-Werror=discarded-array-qualifiers]").count(),
        3,
        "{err}"
    );
}
