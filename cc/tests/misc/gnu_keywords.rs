//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's alternate keyword spellings -- `__inline`, `__volatile__`,
// `__signed__`, `__extension__` and the rest -- are keywords, as in gcc, not
// predefined macros.
//

use crate::common::{compile_and_run_everywhere, preprocess_text, run_c17};

/// `echo __inline | gcc -E -` prints `__inline`, and none of the spellings
/// is a macro to `#ifdef`. Expected text is gcc 13's `-E -P`.
#[test]
fn gnu_keyword_spellings_survive_preprocessing() {
    let src = "__inline __inline__ __volatile__ __volatile __restrict __restrict__ \
               __signed __signed__ __extension__\n\
               #if defined(__inline) || defined(__inline__) || defined(__volatile)\n\
               bad1\n#endif\n\
               #if defined(__restrict__) || defined(__signed) || defined(__extension__)\n\
               bad2\n#endif\n";
    let r = preprocess_text("gnu_kw_pp", src, &["-P"]);
    assert!(r.success, "{}", r.stderr);
    assert_eq!(
        r.stdout.trim(),
        "__inline __inline__ __volatile__ __volatile __restrict __restrict__ \
         __signed __signed__ __extension__"
    );
}

/// Linux's compiler_types.h makes `inline` a macro and writes
/// `#define asm_inline asm __inline`: `__inline` must stay the asm `inline`
/// qualifier, untouched by the program's `inline` macro.
#[test]
fn gnu_inline_keyword_unaffected_by_inline_macro() {
    let src = r#"
#define inline inline __attribute__((__gnu_inline__)) __attribute__((__unused__))
#define asm_inline asm __inline
static inline int g(int x) { return x + 1; }
int main(void) {
    int r = g(1);
    asm_inline volatile("" : "+r"(r));
    __asm__ __inline__ __volatile__("" : "+r"(r));
    __asm __volatile __inline("" : "+r"(r));
    return r == 2 ? 0 : 1;
}
"#;
    compile_and_run_everywhere("gnu_inline_kw", src);
}

/// Each spelling means its keyword, and `__extension__` is taken where gcc
/// takes it: before a declaration at file scope, in a block, in a member
/// list and in a `for`, and as a unary prefix.
#[test]
fn gnu_keyword_spellings_mean_their_keywords() {
    let src = r#"
__extension__ typedef long long ll;
struct s { __extension__ union { int a; }; __extension__ int b; };
static __inline__ int one(void) { return 1; }
static __inline int two(void) { return 2; }
int main(void) {
    __signed__ char c = -1;
    __signed int si = -2;
    int *__restrict__ rp = 0;
    int *__restrict p = &si;
    __volatile__ int v = 3;
    __volatile int w = 4;
    __extension__ int x = __extension__ 5;
    struct s st = { .a = 6, .b = 7 };
    for (__extension__ int i = 0; i < 2; i++) x += i;
    __extension__ x++;
    ll big = __extension__ ({ (ll)1 << 40; });
    if (c != -1 || *p != -2 || rp || v != 3 || w != 4) return 1;
    if (x != 7 || st.a != 6 || st.b != 7 || big != (1LL << 40)) return 2;
    if (one() + two() != 3 || sizeof(__signed__ short) != sizeof(short)) return 3;
    return 0;
}
"#;
    compile_and_run_everywhere("gnu_kw_meaning", src);
}

/// A `.i` from gcc carries the spellings as written, since gcc has no macro
/// for any of them; c17 must compile it without a preprocessor to help.
#[test]
fn gnu_keyword_spellings_in_preprocessed_input() {
    let dir = plib::tmp::Builder::new()
        .prefix("c17_gnu_kw_i_")
        .tempdir()
        .unwrap();
    let i = dir.path().join("t.i");
    std::fs::write(
        &i,
        "__extension__ typedef long long ll;\n\
         static __inline int f(__signed__ int x) { return __extension__ ({ x; }); }\n\
         int g(void) { __asm__ __volatile__ (\"\"); return f(1); }\n",
    )
    .unwrap();
    let o = dir.path().join("t.o");
    let r = run_c17(&["-c", &i.to_string_lossy(), "-o", &o.to_string_lossy()]);
    assert!(r.success, "{}", r.stderr);
}
