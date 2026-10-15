//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU make's target-specific and pattern-specific macros,
//! `targets: [override] [export] name op value`. sysvinit links each program
//! with `init: LDLIBS += $(INITLIBS) $(STATIC)`; libzstd sets its flags per
//! library (`$(LIBZSTD): CFLAGS += -fPIC`) and per pattern
//! (`%-mt : CPPFLAGS_DYNLIB := -DZSTD_MULTITHREAD`).
//!
//! The assignment is in force while the target's commands are expanded, and
//! those of every target made as its prerequisite, unless that target
//! assigns the name itself. Every expectation here was checked against GNU
//! make.

use crate::common::Dir;

fn run(name: &str, makefile: &str, args: &[&str], expected: &str) {
    let dir = Dir::new(name);
    dir.write("Makefile", makefile);
    dir.make(args).ok(expected);
}

#[test]
fn a_target_sees_its_own_value() {
    run(
        "tv_basic",
        "CFLAGS = -O\nall: foo bar\nfoo: CFLAGS = -g\nfoo bar:\n\t@echo $@ $(CFLAGS)\n",
        &[],
        "foo -g\nbar -O\n",
    );
}

#[test]
fn prerequisites_inherit_the_value() {
    run(
        "tv_inherit",
        "all: X = parent\nall: dep\n\t@echo all $(X)\ndep:\n\t@echo dep $(X)\n",
        &[],
        "dep parent\nall parent\n",
    );
}

// sysvinit's src/Makefile.
#[test]
fn sysvinit_appends_to_ldlibs() {
    run(
        "tv_sysvinit",
        "LDLIBS = -lc\nSTATIC =\nall: init halt\n\
         init:\t\tLDLIBS += -lcrypt $(STATIC)\n\
         init halt:\n\t@echo $@ [$(LDLIBS)]\n",
        &["STATIC=-static"],
        "init [-lc -lcrypt -static]\nhalt [-lc]\n",
    );
}

#[test]
fn a_command_line_macro_wins_unless_override() {
    run(
        "tv_cmdline",
        "CFLAGS = mk\nfoo: CFLAGS += x\nbar: override CFLAGS += y\n\
         all: foo bar\nfoo bar:\n\t@echo $@ $(CFLAGS)\n",
        &["CFLAGS=cmd", "all"],
        "foo cmd\nbar cmd y\n",
    );
}

// libzstd's lib/Makefile.
#[test]
fn a_pattern_specific_value() {
    run(
        "tv_pattern",
        "%-mt: X := mt\n%-mt: Y = $(X)y\nlib-mt:\n\t@echo $(X) $(Y)\n",
        &[],
        "mt mty\n",
    );
}

// `:=` is expanded where it is read; `=` when the commands are.
#[test]
fn simple_and_recursive_values() {
    run(
        "tv_simple",
        "foo: X := [$(Y)]\nfoo: Z = [$(Y)]\nY = later\nfoo:\n\t@echo $(X) $(Z)\n",
        &[],
        "[] [later]\n",
    );
}

#[test]
fn a_conditional_assignment() {
    run(
        "tv_qmark",
        "X = global\nfoo: X ?= a\nfoo: W ?= w\nfoo:\n\t@echo $(X) $(W)\n",
        &[],
        "global w\n",
    );
}

#[test]
fn an_exported_value_reaches_the_environment() {
    run(
        "tv_export",
        "foo: export X = 1\nfoo: Y = 2\nfoo:\n\t@echo [$$X] [$$Y]\n",
        &[],
        "[1] []\n",
    );
}

// libzstd: `libzstd.a: CPPFLAGS += $(CPPFLAGS_STATICLIB)` over a
// prerequisite's own `+=`.
#[test]
fn appends_accumulate_down_the_graph() {
    run(
        "tv_accumulate",
        "all: CPPFLAGS += -DA\nall: lib\nlib: CPPFLAGS += -DB\nlib:\n\t@echo [$(CPPFLAGS)]\n",
        &[],
        "[-DA -DB]\n",
    );
}

// A macro that refers to the assigned one sees the target's value: libzstd's
// `FLAGS = $(CPPFLAGS) $(CFLAGS) ...`.
#[test]
fn a_reference_sees_the_target_value() {
    run(
        "tv_reference",
        "FLAGS = $(CPPFLAGS) $(CFLAGS)\nCFLAGS = -O2\nfoo: CFLAGS += -fPIC\nfoo:\n\t@echo $(FLAGS)\n",
        &[],
        "-O2 -fPIC\n",
    );
}

#[test]
fn several_targets_share_an_assignment() {
    run(
        "tv_multi",
        "a b: V = shared\na b:\n\t@echo $@ $(V)\n",
        &["a", "b"],
        "a shared\nb shared\n",
    );
}

// A target-specific value is used in a `$(shell ...)` in the commands too:
// libzstd computes its build directory from the target's flags.
#[test]
fn a_shell_call_in_the_commands_sees_the_target_value() {
    run(
        "tv_shell",
        "CFLAGS = -O\nfoo: CFLAGS += -g\nfoo bar:\n\t@echo $@ $(shell echo $(CFLAGS) | tr ' ' _)\n",
        &["foo", "bar"],
        "foo -O_-g\nbar -O\n",
    );
}

// The command after a rule line's `;` is expanded when it runs, like any
// other, so it sees macros defined after the rule.
#[test]
fn an_inline_command_is_expanded_when_it_runs() {
    run(
        "tv_inline",
        "X = global\nall: ; @echo $(X) $(Y)\nY = late\n",
        &[],
        "global late\n",
    );
}

// A command line that expands to nothing is neither shown nor run.
#[test]
fn an_empty_command_line_is_skipped() {
    run(
        "tv_empty_command",
        "all:\n\t$(E)\n\t@echo hi\n",
        &["-n"],
        "echo hi\n",
    );
}

// libselinux's utils/Makefile: `override LDLIBS += -lselinux` and then
// `sefcontext_compile: LDLIBS += ../src/libselinux.a`. A global `override`
// does not stop a target-specific assignment: only a command-line macro (or
// the environment under -e) replaces one not itself written `override`.
const OVERRIDE_GLOBAL: &str = "override L += -lsel\n\
     t: L += extra\nt2: L = set\nt3: L := imm\nt4: L ?= q\n\
     t5: override L += ov\n%.p: L += pat\n\
     all: t t2 t3 t4 t5 x.p\nt t2 t3 t4 t5 x.p:\n\t@echo \"$@: [$(L)]\"\n";

#[test]
fn a_target_value_applies_over_a_global_override() {
    run(
        "tv_over_override",
        OVERRIDE_GLOBAL,
        &[],
        "t: [-lsel extra]\nt2: [set]\nt3: [imm]\nt4: [-lsel]\nt5: [-lsel ov]\nx.p: [-lsel pat]\n",
    );
}

#[test]
fn a_global_override_lets_target_values_beat_the_command_line() {
    run(
        "tv_over_override_cmd",
        OVERRIDE_GLOBAL,
        &["L=cmd"],
        "t: [cmd -lsel extra]\nt2: [set]\nt3: [imm]\nt4: [cmd -lsel]\n\
         t5: [cmd -lsel ov]\nx.p: [cmd -lsel pat]\n",
    );
}

// A prerequisite's own assignment wins over an `override` it inherits; a
// command-line macro replaces it, inherited `override` or not.
const INHERITED: &str = "L = base\np: override L += ov\nc: L = x\np2: L = pset\n\
     c2: L += cadd\np: c\np2: c2\np c p2 c2:\n\t@echo \"$@: [$(L)]\"\n";

#[test]
fn an_own_value_beats_an_inherited_override() {
    run(
        "tv_inherit_override",
        INHERITED,
        &["p", "p2"],
        "c: [x]\np: [base ov]\nc2: [pset cadd]\np2: [pset]\n",
    );
}

#[test]
fn a_command_line_macro_replaces_an_inherited_override() {
    run(
        "tv_inherit_override_cmd",
        INHERITED,
        &["L=cmd", "p", "p2"],
        "c: [cmd]\np: [cmd ov]\nc2: [cmd]\np2: [cmd]\n",
    );
}

#[test]
fn the_environment_under_dash_e_replaces_a_target_value() {
    let dir = Dir::new("tv_env_e");
    dir.write(
        "Makefile",
        "L = base\nt: L += extra\nt2: L = set\nt5: override L += ov\n\
         all: t t2 t5\nt t2 t5:\n\t@echo \"$@: [$(L)]\"\n",
    );
    dir.make_env(&["-e"], &[("L", "env")])
        .ok("t: [env]\nt2: [env]\nt5: [env ov]\n");
    dir.make_env(&[], &[("L", "env")])
        .ok("t: [base extra]\nt2: [set]\nt5: [base ov]\n");
}
