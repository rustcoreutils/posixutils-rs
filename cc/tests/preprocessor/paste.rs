//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `##` with an empty operand (C17 6.10.3.3p2 placemarkers) and with
// `__VA_ARGS__` as an operand. Expected text is gcc 13's `-E -P` output.
//

use crate::common::{compile_and_run_everywhere, preprocess_text};

/// Preprocess `src` with `-P` and require exactly `want`.
#[track_caller]
fn assert_pp(name: &str, src: &str, want: &str) {
    let run = preprocess_text(name, src, &["-P"]);
    assert!(run.success, "{name}: {}", run.stderr);
    assert!(run.stderr.is_empty(), "{name}: {}", run.stderr);
    let got: Vec<&str> = run.stdout.lines().filter(|l| !l.is_empty()).collect();
    let want: Vec<&str> = want.lines().collect();
    assert_eq!(got, want, "{name}");
}

/// An empty argument on either side of `##`, on both, and inside a chain.
/// The paste must not reach the unrelated token before the operand, and the
/// operand that stands takes the placemarker's spacing.
#[test]
fn paste_empty_argument_is_placemarker() {
    let src = "#define INC(x) ((x) += 1)\n\
               #define C(mod, v) { mod##INC(v); }\n\
               #define R(a, b) [a ## b]\n\
               #define CAT3(a, b, c) <a ## b ## c>\n\
               C(, v)\nC(PRE_, v)\n\
               R(x, )\nR(, y)\nR(, )\nR(x, y)\n\
               CAT3(, , )\nCAT3(a, , )\nCAT3(, b, )\nCAT3(, , c)\n\
               CAT3(a, , c)\nCAT3(, b, c)\nCAT3(a, b, c)\n\
               f(1) C(,w) g\n";
    let want = "{ ((v) += 1); }\n{ PRE_INC(v); }\n\
                [x]\n[y]\n[]\n[xy]\n\
                <>\n<a>\n<b>\n<c>\n<ac>\n<bc>\n<abc>\n\
                f(1) { ((w) += 1); } g";
    assert_pp("paste_empty", src, want);
}

/// `name ## __VA_ARGS__` pastes the first variadic token onto `name`,
/// `__VA_ARGS__ ## name` the last, and an empty `__VA_ARGS__` is a
/// placemarker. The GNU `, ## __VA_ARGS__` comma elision is unchanged.
#[test]
fn paste_va_args_operand() {
    let src = "#define SEL(n, ...) name ## __VA_ARGS__ (n)\n\
               #define SUF(n, ...) __VA_ARGS__ ## suffix (n)\n\
               #define BOTH(...) pre ## __VA_ARGS__ ## post\n\
               #define ELIDE(fmt, ...) printf(fmt, ## __VA_ARGS__)\n\
               #define ELIDE2(fmt, args...) printf(fmt, ## args)\n\
               x SEL(1)\nx SEL(1, _a)\nx SEL(1, _a, b)\n\
               x SUF(1)\nx SUF(1, my_)\nx SUF(1, a, my_)\n\
               x BOTH()\nx BOTH(mid)\nx BOTH(a, b)\n\
               ELIDE(\"x\")\nELIDE(\"x\", 1, 2)\nELIDE2(\"y\")\nELIDE2(\"y\", 3)\n";
    let want = "x name (1)\nx name_a (1)\nx name_a, b (1)\n\
                x suffix (1)\nx my_suffix (1)\nx a, my_suffix (1)\n\
                x prepost\nx premidpost\nx prea, bpost\n\
                printf(\"x\")\nprintf(\"x\", 1, 2)\nprintf(\"y\")\nprintf(\"y\", 3)";
    assert_pp("paste_va_args", src, want);
}

/// Linux include/net/ipv6.h `_DEVINC`: `mod##INC(v)` with `mod` empty, after
/// a `)` on the line before.
#[test]
fn paste_empty_argument_runs() {
    let src = r#"
#define INC(x) ((x) += 1)
#define _DEVINC(v, mod, cond)            \
({                                       \
	if (cond)                        \
		mod##INC(v);             \
	mod##INC(v);                     \
})
int main(void) { int a = 0; _DEVINC(a, , a == 0); return a == 2 ? 0 : 1; }
"#;
    compile_and_run_everywhere("paste_empty_runs", src);
}

/// AppArmor label.h: `name ## __VA_ARGS__` selects a macro by its suffix.
#[test]
fn paste_va_args_runs() {
    let src = r#"
#define label_for_each(I, N) for ((I) = 0; (I) < (N); (I)++)
#define label_for_each_confined(I, N) for ((I) = 0; (I) < (N) - 1; (I)++)
#define fn_for_each_XXX(N, ...) \
    ({ int i, c = 0; label_for_each ## __VA_ARGS__(i, N) c++; c; })
int main(void) {
    return !(fn_for_each_XXX(5) == 5 && fn_for_each_XXX(5, _confined) == 4);
}
"#;
    compile_and_run_everywhere("paste_va_args_runs", src);
}
