//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX 105578-105607: an included file that a rule can make is made, and
//! read once it has been ("delayed remaking").

use crate::common::Dir;

#[test]
fn a_missing_include_is_made_and_read() {
    let dir = Dir::new("include_made");
    dir.write(
        "Makefile",
        "all:\n\t@echo all [$(VAR)]\ninclude inc.mk\n\
         inc.mk:\n\t@echo making inc.mk\n\t@echo 'VAR = from inc' > inc.mk\n",
    );
    dir.make(&[]).ok("making inc.mk\nall [from inc]\n");
    // Now it exists, and has no prerequisites: nothing to make.
    dir.make(&[]).ok("all [from inc]\n");
}

#[test]
fn an_out_of_date_include_is_remade() {
    let dir = Dir::new("include_stale");
    dir.write(
        "Makefile",
        "all:\n\t@echo all [$(VAR)]\ninclude inc.mk\n\
         inc.mk: inc.in\n\t@echo regen\n\t@cp inc.in inc.mk\n",
    );
    dir.write("inc.mk", "VAR = old\n");
    let old = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000);
    std::fs::File::options()
        .write(true)
        .open(dir.path("inc.mk"))
        .unwrap()
        .set_modified(old)
        .unwrap();
    dir.write("inc.in", "VAR = new\n");
    dir.make(&[]).ok("regen\nall [new]\n");
}

// The makefile is what is being made, so its recipe runs even under -n and
// is shown; GNU make does the same.
#[test]
fn an_include_is_made_even_under_dash_n() {
    let dir = Dir::new("include_dash_n");
    dir.write(
        "Makefile",
        "all:\n\techo all [$(VAR)]\ninclude inc.mk\n\
         inc.mk:\n\techo 'VAR = made' > inc.mk\n",
    );
    dir.make(&["-n"])
        .ok("echo 'VAR = made' > inc.mk\necho all [made]\n");
    assert!(dir.path("inc.mk").exists());
}

// automake's dependency tracking: `include ./$(DEPDIR)/foo.Po` with a rule
// that creates the file.
#[test]
fn automake_depfiles_are_created() {
    let dir = Dir::new("include_depfiles");
    dir.write(
        "Makefile",
        "DEPDIR = .deps\n\
         am__depfiles_remade = ./$(DEPDIR)/a.Po ./$(DEPDIR)/b.Po\n\
         all:\n\t@echo all\n\
         include ./$(DEPDIR)/a.Po # am--include-marker\n\
         include ./$(DEPDIR)/b.Po # am--include-marker\n\
         $(am__depfiles_remade):\n\
         \t@mkdir -p $(@D)\n\
         \t@echo '# dummy' >$@-t && mv -f $@-t $@\n\
         am--depfiles: $(am__depfiles_remade)\n",
    );
    dir.make(&[]).ok("all\n");
    assert!(dir.path(".deps/a.Po").exists());
    assert!(dir.path(".deps/b.Po").exists());
}

#[test]
fn a_dash_include_is_made_when_a_rule_can() {
    let dir = Dir::new("include_dash_made");
    dir.write(
        "Makefile",
        "all:\n\t@echo all [$(VAR)]\n-include inc.mk\ninc.mk:\n\t@echo 'VAR = made' > inc.mk\n",
    );
    dir.make(&[]).ok("all [made]\n");
}

#[test]
fn a_dash_include_nothing_makes_is_ignored() {
    let dir = Dir::new("include_dash_none");
    dir.write("Makefile", "all:\n\t@echo all\n-include nosuch.mk\n");
    dir.make(&[]).ok("all\n");
}

// A rule that makes nothing must not make make read the makefile again
// forever: libzstd's `$(DEPFILES):` has no commands.
#[test]
fn a_rule_that_makes_nothing_does_not_loop() {
    let dir = Dir::new("include_no_change");
    dir.write("Makefile", "all:\n\t@echo all\n-include a.d\na.d:\n");
    dir.make(&[]).ok("all\n");
}

#[test]
fn a_missing_include_nothing_makes_is_an_error() {
    let dir = Dir::new("include_missing");
    dir.write("Makefile", "all:\n\t@echo all\ninclude nosuch.mk\n");
    let run = dir.make(&[]);
    assert_ne!(run.code, Some(0));
    assert_eq!(run.stdout, "");
    assert!(run.stderr.contains("nosuch.mk"), "stderr: {}", run.stderr);
}

// POSIX 105572-105577: the line names one or more pathnames, separated by
// blanks.
#[test]
fn an_include_line_names_several_files() {
    let dir = Dir::new("include_several");
    dir.write("a.mk", "A = 1\n");
    dir.write("b.mk", "B = 2\n");
    dir.write(
        "Makefile",
        "FILES = a.mk b.mk\ninclude $(FILES)\nall:\n\t@echo $(A) $(B)\n",
    );
    dir.make(&[]).ok("1 2\n");
}

#[test]
fn a_dash_include_of_a_wildcard_reads_every_file() {
    let dir = Dir::new("include_wildcard");
    dir.write("x.d", "X = x\n");
    dir.write("y.d", "Y = y\n");
    dir.write(
        "Makefile",
        "-include $(wildcard *.d)\nall:\n\t@echo $(X)$(Y)\n",
    );
    dir.make(&[]).ok("xy\n");
}
