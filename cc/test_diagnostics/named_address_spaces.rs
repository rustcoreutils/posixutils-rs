//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc 13's verdicts on x86-64's named address spaces, `__seg_fs` and
// `__seg_gs`: the generic space and each segment are disjoint, so a
// pointer converts between them only by a cast, which warns; an object of
// automatic storage, a parameter or a member cannot be in one; and one type
// is in one space at most.
//

use crate::test_compile::{compile, compile_accepted};

const X86: &str = "--target=x86_64-unknown-linux-gnu";

/// Each diagnostic of `src` as `line: severity: message`, columns aside.
fn diagnostics(name: &str, src: &str) -> Vec<String> {
    let c = compile(name, src, &[X86]);
    let prefix = format!("{name}.c:");
    c.stderr
        .lines()
        .filter_map(|l| l.strip_prefix(&prefix))
        .filter_map(|l| {
            let (line, rest) = l.split_once(':')?;
            let (_, rest) = rest.split_once(": ")?;
            Some(format!("{line}: {rest}"))
        })
        .collect()
}

#[test]
fn named_address_space_conversions_are_gcc_verdicts() {
    let src = "\
void take(int *p);
void takes(int __seg_gs *p);
int *f(int *g, int __seg_gs *s, int __seg_fs *fs) {
  int *a = s;
  int __seg_gs *b = g;
  a = s;
  b = fs;
  take(s);
  takes(g);
  (void)a; (void)b;
  return s;
}
int cmp(int __seg_gs *a, int *b) { return a == b; }
long sdiff(int __seg_gs *a, int *b) { return a - b; }
int scond(int c, int __seg_gs *a, int *b) { return *(c ? a : b); }
int *g2s(int *p) { return (int *)(int __seg_gs *)p; }
int __seg_gs *s2s(int __seg_fs *p) { return (int __seg_gs *)p; }
";
    assert_eq!(
        diagnostics("named_as_conv", src),
        [
            "4: error: initialization from pointer to non-enclosed address space",
            "4: note: expected 'int *' but pointer is of type '__seg_gs int *'",
            "5: error: initialization from pointer to non-enclosed address space",
            "5: note: expected '__seg_gs int *' but pointer is of type 'int *'",
            "6: error: assignment from pointer to non-enclosed address space",
            "6: note: expected 'int *' but pointer is of type '__seg_gs int *'",
            "7: error: assignment from pointer to non-enclosed address space",
            "7: note: expected '__seg_gs int *' but pointer is of type '__seg_fs int *'",
            "8: error: passing argument 1 of 'take' from pointer to non-enclosed address space",
            "1: note: expected 'int *' but argument is of type '__seg_gs int *'",
            "9: error: passing argument 1 of 'takes' from pointer to non-enclosed address space",
            "2: note: expected '__seg_gs int *' but argument is of type 'int *'",
            "11: error: return from pointer to non-enclosed address space",
            "11: note: expected 'int *' but pointer is of type '__seg_gs int *'",
            "13: error: comparison of pointers to disjoint address spaces",
            "14: error: invalid operands to binary - (have '__seg_gs int *' and 'int *')",
            "15: error: pointers to disjoint address spaces used in conditional expression",
            "16: warning: cast to '__seg_gs' address space pointer from disjoint generic \
             address space pointer",
            "16: warning: cast to generic address space pointer from disjoint '__seg_gs' \
             address space pointer",
            "17: warning: cast to '__seg_gs' address space pointer from disjoint '__seg_fs' \
             address space pointer",
        ]
    );
}

#[test]
fn named_address_space_objects_are_gcc_verdicts() {
    let src = "\
struct m { int __seg_gs field; };
void param(int __seg_gs x);
int f(void) { int __seg_gs local; return local; }
int g(void) { int __seg_gs arr[2] = { 0 }; return arr[0]; }
int __seg_gs __seg_fs both;
";
    assert_eq!(
        diagnostics("named_as_obj", src),
        [
            "1: error: '__seg_gs' specified for structure field 'field'",
            "2: error: '__seg_gs' specified for parameter 'x'",
            "3: error: '__seg_gs' specified for auto variable 'local'",
            "4: error: '__seg_gs' specified for auto variable 'arr'",
            "5: error: incompatible address space qualifiers '__seg_fs' and '__seg_gs'",
        ]
    );
}

/// What gcc accepts in silence: an integer or a null pointer constant made
/// a segment pointer, a segment pointer made an integer, `void *` in the
/// same space, static and external objects in a segment, a function
/// returning a value read from one, typedefs and `sizeof`.
#[test]
fn named_address_space_valid_uses_are_silent() {
    let src = "\
typedef int __seg_gs gsint;
gsint *tp;
int __seg_gs arr[4];
long s2l(int __seg_gs *p) { return (long)p; }
int __seg_gs *l2s(long v) { return (int __seg_gs *)v; }
void __seg_gs *s2sv(int __seg_gs *p) { return p; }
int __seg_gs *sv2s(void __seg_gs *p) { return p; }
int null(int __seg_gs *p) { return p == 0 || p == tp; }
int __seg_gs *setnull(void) { return 0; }
long diff(int __seg_gs *a, gsint *b) { return a - b; }
int lstatic(void) { static int __seg_gs s; extern int __seg_gs e; return s + e; }
const volatile __seg_fs unsigned long *cv;
_Static_assert(sizeof(int __seg_gs) == 4, \"s\");
int __seg_gs *const *pp;
";
    let stderr = compile_accepted("named_as_ok", src, &[X86, "-Wall"]);
    assert!(stderr.is_empty(), "{stderr}");
}

/// What gcc reaches through a segment and c17 does not yet is refused, never
/// compiled to an access of the generic address: atomics, a 128-bit
/// integer and a `long double`.
#[test]
fn named_address_space_unsupported_accesses_are_refused() {
    let sorry =
        |what: &str| format!("error: sorry, unimplemented: {what} in a named address space");
    let atomics = "\
int at(int __seg_gs *p) { return __atomic_fetch_add(p, 1, 5); }
int cas(int __seg_gs *p) { return __sync_val_compare_and_swap(p, 1, 2); }
";
    let wide = "\
__int128 wide(__int128 __seg_gs *p) { return *p; }
long double ld(long double __seg_fs *p) { return *p; }
";
    for (name, src, want) in [
        (
            "named_as_atomic",
            atomics,
            [(1, "an atomic operation"), (2, "an atomic operation")],
        ),
        (
            "named_as_wide",
            wide,
            [(1, "a 128-bit integer load"), (2, "a long double load")],
        ),
    ] {
        let got = diagnostics(name, src);
        for (line, what) in want {
            let want = format!("{line}: {}", sorry(what));
            assert!(got.contains(&want), "{want}\n{got:?}");
        }
    }
}

/// The names are x86-64's: no other target has them.
#[test]
fn named_address_spaces_are_x86_64_only() {
    let c = compile(
        "named_as_aarch64",
        "int __seg_gs x;\n",
        &["--target=aarch64-unknown-linux-gnu"],
    );
    assert!(!c.success, "{}", c.stderr);
}
