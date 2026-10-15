//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! How a macro reference expands: once, recursively, and never rescanned.
//!
//! POSIX 105776-105784: a delayed-expansion macro is expanded when it is
//! referenced and its result scanned for further macros; an
//! immediate-expansion macro's value is not scanned again. What a function
//! returns is data, not makefile text: a `$` in `$(shell ...)` output or in
//! a file name is a `$`. Every expected output here is GNU make 4.3's,
//! except `:::=`, which GNU 4.3 does not have and which follows POSIX
//! 105739-105740.

use crate::common::Dir;

/// `$(shell ...)` output is not scanned for macros.
#[test]
fn shell_output_is_not_expanded_again() {
    let dir = Dir::new("expand_shell_data");
    dir.write(
        "Makefile",
        "A := $(shell echo '$$x')\nR = $(shell printf '%s' '$$(B)')\nB = bee\n\
         all:\n\t@echo '$(A)' '$(R)'\n\t@echo '$(shell printf '%s' '$$(B)')'\n",
    );
    dir.make(&[]).ok("$x $(B)\n$(B)\n");
}

/// A function body's `$$$$` reaches the shell as `$$`, and the `$$` the
/// shell writes back is two dollars.
#[test]
fn shell_output_from_a_call_keeps_its_dollars() {
    let dir = Dir::new("expand_shell_call");
    dir.write(
        "Makefile",
        "f = $(shell printf '%s' '$$$$1')\nall:\n\t@echo '$(call f,z)'\n",
    );
    dir.make(&[]).ok("$$1\n");
}

/// POSIX 105741-105749: `!=` defines a delayed-expansion macro, so unlike
/// `$(shell ...)` its value is expanded when it is used.
#[test]
fn bang_output_is_expanded_when_used() {
    let dir = Dir::new("expand_bang");
    dir.write(
        "Makefile",
        "A != printf '%s' '$$(B)'\nB = bee\nall:\n\t@echo '$(A)'\n",
    );
    dir.make(&[]).ok("bee\n");
}

/// A file name `$(wildcard ...)` or `$(realpath ...)` finds is a name.
#[test]
fn found_file_names_are_not_expanded() {
    let dir = Dir::new("expand_file_data");
    dir.write("x$(B)y", "").write("r$(B)", "").write(
        "Makefile",
        "B = bee\nall:\n\t@echo '$(wildcard x*y)' '$(notdir $(realpath r$$(B)))'\n",
    );
    dir.make(&[]).ok("x$(B)y r$(B)\n");
}

/// The automatic macros name files, and a file name is not makefile text,
/// inside a function call or a substitution as much as alone.
#[test]
#[ignore = "fails until expansion is single-pass"]
fn automatic_macro_values_are_not_expanded() {
    let dir = Dir::new("expand_automatic_data");
    dir.write("x$(B).c", "").write(
        "Makefile",
        "B = bee\n%.o: %.c\n\t@echo '$<' '$(notdir $<)' '$(<:.c=.h)' '$(basename $@)'\n",
    );
    dir.make(&["x$(B).o"]).ok("x$(B).c x$(B).c x$(B).h x$(B)\n");
}

/// A function reading an automatic macro through another macro sees the
/// macro's value.
#[test]
#[ignore = "fails until expansion is single-pass"]
fn a_function_sees_an_automatic_macro_through_a_macro() {
    let dir = Dir::new("expand_automatic_indirect");
    dir.write(
        "Makefile",
        "X = $(notdir $@)\nall:\n\t@echo '$(X:.c=.o)' '$(subst a,b,$(X))'\n",
    );
    dir.make(&[]).ok("all bll\n");
}

/// POSIX 105759-105761: appending to an immediate-expansion macro appends
/// the evaluation of the new text, now.
#[test]
fn appending_to_an_immediate_macro_expands_now() {
    let dir = Dir::new("expand_append_simple");
    dir.write(
        "Makefile",
        "X = one\nA := a\nA += $(X)\nX = two\nall:\n\t@echo $(A)\n",
    );
    dir.make(&[]).ok("a one\n");
}

/// POSIX 105762-105764: appending to a delayed-expansion macro appends the
/// text unevaluated.
#[test]
fn appending_to_a_delayed_macro_expands_later() {
    let dir = Dir::new("expand_append_recursive");
    dir.write(
        "Makefile",
        "X = one\nA = a\nA += $(X)\nX = two\nall:\n\t@echo $(A)\n",
    );
    dir.make(&[]).ok("a two\n");
}

/// A target-specific `+=` is expanded when the target's commands are, even
/// onto an immediate-expansion macro, as in GNU make.
#[test]
fn a_target_specific_append_expands_when_used() {
    let dir = Dir::new("expand_append_target");
    dir.write(
        "Makefile",
        "X = one\nA := a\nall: A += $(X)\nX = two\nall:\n\t@echo $(A)\n",
    );
    dir.make(&[]).ok("a two\n");
}

/// `define NAME :=` is an immediate-expansion definition.
#[test]
fn define_honours_its_operator() {
    let dir = Dir::new("expand_define_simple");
    dir.write(
        "Makefile",
        "X = one\ndefine A :=\n$(X)\nendef\ndefine B =\n$(X)\nendef\nX = two\n\
         all:\n\t@echo $(A) $(B)\n",
    );
    dir.make(&[]).ok("one two\n");
}

/// An immediate-expansion macro keeps the `$` that `$$` gave it.
#[test]
fn an_immediate_macro_keeps_an_escaped_dollar() {
    let dir = Dir::new("expand_simple_dollar");
    dir.write(
        "Makefile",
        "A := $$(B)\nB = bee\nL := $(foreach v,a b,$$$$($(v)))\n\
         all:\n\t@echo '$(A)' '$(L)'\n",
    );
    dir.make(&[]).ok("$(B) $$(a) $$(b)\n");
}

/// POSIX 105739-105740 and 105799-105801: `:::=` expands now, leaving `$$`
/// as it is, and defines a delayed-expansion macro.
#[test]
fn colon3_expands_now_and_again_when_used() {
    let dir = Dir::new("expand_colon3");
    dir.write(
        "Makefile",
        "X = one\nA :::= $(X) $$(Y)\nX = two\nY = why\nall:\n\t@echo '$(A)'\n",
    );
    dir.make(&[]).ok("one $(Y)\n");
}

/// POSIX 105830-105832: macro expansions on the right-hand side of the `:`
/// are expanded before it is examined.
#[test]
fn a_substitution_expands_its_replacement() {
    let dir = Dir::new("expand_subst_nested");
    dir.write(
        "Makefile",
        "D = obj\nSRC = a.c b.c\nall:\n\t@echo $(SRC:%.c=$(D)/%.o)\n",
    );
    dir.make(&[]).ok("obj/a.o obj/b.o\n");
}

/// ... including the `=` and the `%` of the substitution.
#[test]
fn a_substitution_can_be_computed() {
    let dir = Dir::new("expand_subst_computed");
    dir.write(
        "Makefile",
        "S = .c=.o\nP = %\nSRC = a.c b.c\nall:\n\t@echo $(SRC:$(S)) $(SRC:$(P).c=$(P).s)\n",
    );
    dir.make(&[]).ok("a.o b.o a.s b.s\n");
}

/// POSIX 105785 asks for at least 100 levels of indirection; GNU make has
/// no limit.
#[test]
fn a_long_chain_of_macros_expands() {
    let dir = Dir::new("expand_deep_chain");
    let mut makefile = String::new();
    for i in 1..=300 {
        makefile.push_str(&format!("V{i} = $(V{})\n", i + 1));
    }
    makefile.push_str("V301 = end\nall: $(V1)\n\t@echo $(V1)\nend:\n\t@:\n");
    dir.write("Makefile", &makefile);
    dir.make(&[]).ok("end\n");
}

/// A macro whose value refers to itself is an error, named, when used.
#[test]
#[ignore = "fails until self-reference is detected"]
fn a_self_referencing_macro_is_named() {
    let dir = Dir::new("expand_self_reference");
    dir.write("Makefile", "A = $(A)x\nall:\n\t@echo $(A)\n");
    let run = dir.make(&[]);
    assert_ne!(run.code, Some(0), "{run:?}");
    assert!(
        run.stderr
            .contains("Recursive variable 'A' references itself (eventually)"),
        "{run:?}"
    );

    dir.write("Makefile", "A = $(B)\nB = x$(A)\nall: $(A)\n\t@echo hi\n");
    let run = dir.make(&[]);
    assert_ne!(run.code, Some(0), "{run:?}");
    assert!(
        run.stderr
            .contains("Recursive variable 'A' references itself (eventually)"),
        "{run:?}"
    );
}

/// ... but only when used.
#[test]
fn an_unused_self_referencing_macro_is_harmless() {
    let dir = Dir::new("expand_self_reference_unused");
    dir.write("Makefile", "A = $(A)x\nall:\n\t@echo fine\n");
    dir.make(&[]).ok("fine\n");
}

/// What `$(warning ...)` writes is expanded text, with `$$` a `$`.
#[test]
fn a_warning_shows_a_dollar() {
    let dir = Dir::new("expand_warning_dollar");
    dir.write("Makefile", "$(warning w$$x)\nall:\n\t@:\n");
    let run = dir.make(&[]);
    run.ok("");
    assert!(run.stderr.contains("w$x"), "{run:?}");
    assert!(!run.stderr.contains("w$$x"), "{run:?}");
}

/// `$(eval ...)` reads text with one level of `$` consumed.
#[test]
fn eval_consumes_one_level_of_dollars() {
    let dir = Dir::new("expand_eval_dollar");
    dir.write(
        "Makefile",
        "define T\n$(1)_V = $$(X)\nendef\nX = ex\n$(eval $(call T,p))\n\
         all:\n\t@echo $(p_V)\n",
    );
    dir.make(&[]).ok("ex\n");
}
