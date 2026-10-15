//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What hand-written upstream Makefiles take from GNU make's built-in rules
//! and pattern rule search. libzstd compiles with `$(COMPILE.c)
//! $(OUTPUT_OPTION) $<` from a `%.o: %.c` or `%.o: %.S` rule chosen by which
//! source exists (found through `vpath`), and builds `lib-release` from
//! `%-release : %`; sysvinit links `init: init.o ...` with no commands of
//! its own and sets `CPPFLAGS` for the built-in compile rule.

use crate::common::Dir;

#[test]
fn compile_c_and_output_option() {
    let dir = Dir::new("up_compile_c");
    dir.write(
        "Makefile",
        "CC = cc\nCFLAGS = -O2\nCPPFLAGS = -DX\nall:\n\t@echo $(COMPILE.c) $(OUTPUT_OPTION) x.c\n\t@echo $(COMPILE.S)\n\t@echo $(RM)\n",
    );
    dir.make(&[])
        .ok("cc -O2 -DX -c -o all x.c\ncc -DX -c\nrm -f\n");
}

// libzstd's tests/Makefile links every test program with `$(LINK.c) $^ -o
// $@`; without the macro the shell was asked to run `datagen.c`.
#[test]
fn link_c() {
    let dir = Dir::new("up_link_c");
    dir.write(
        "Makefile",
        "CC = cc\nCFLAGS = -O2\nCPPFLAGS = -DX\nLDFLAGS = -s\nall:\n\t@echo $(LINK.c) a.c -o a\n",
    );
    dir.make(&[]).ok("cc -O2 -DX -s a.c -o a\n");
}

// The first pattern rule whose prerequisites exist, here through `vpath`.
#[test]
fn a_pattern_rule_is_chosen_by_which_prerequisite_exists() {
    let dir = Dir::new("up_pattern_choice");
    dir.write(
        "Makefile",
        "vpath %.c src\nvpath %.S src\nall: obj/a.o obj/b.o\n\
         obj/%.o: %.c\n\t@echo C $< $@\nobj/%.o: %.S\n\t@echo AS $< $@\n",
    );
    dir.write("src/a.c", "");
    dir.write("src/b.S", "");
    dir.make(&[]).ok("C src/a.c obj/a.o\nAS src/b.S obj/b.o\n");
}

// A prerequisite that some rule names "ought to exist" (GNU make): libzstd's
// `$(BUILD_DIR)/%.o : %.c $(BUILD_DIR)/%.d` with `$(DEPFILES):`.
#[test]
fn a_prerequisite_another_rule_names_ought_to_exist() {
    let dir = Dir::new("up_ought");
    dir.write(
        "Makefile",
        "all: o/a.o\no/%.o: %.c o/%.d\n\t@echo cc $< [$*]\no/a.d:\n",
    );
    dir.write("a.c", "");
    dir.make(&[]).ok("cc a.c [a]\n");
}

// A target with a rule but no commands takes them from a pattern rule.
#[test]
fn an_explicit_target_without_commands_uses_a_pattern_rule() {
    let dir = Dir::new("up_release");
    dir.write(
        "Makefile",
        "lib-release:\n%-release: DEBUGFLAGS :=\n%-release: %\n\t@echo release build completed [$(DEBUGFLAGS)]\n\
         DEBUGFLAGS = -Wall\nlib:\n\t@echo lib [$(DEBUGFLAGS)]\n",
    );
    dir.make(&[]).ok("lib []\nrelease build completed []\n");
}

// sysvinit: `init: init.o util.o` has no commands; the built-in rules link
// it from its objects, and compile each object with `CPPFLAGS`.
#[test]
fn a_program_is_linked_from_its_objects() {
    let dir = Dir::new("up_link");
    dir.write(
        "Makefile",
        "CC = cc\nCFLAGS = -O2\nall: init killall5\n\
         init: LDLIBS += -lcrypt\ninit: init.o util.o\n\
         init.o: CPPFLAGS += -DWITH_SELINUX\n",
    );
    for f in ["init.c", "util.c", "killall5.c"] {
        dir.write(f, "");
    }
    dir.make(&["-n"]).ok("cc -O2 -DWITH_SELINUX -c init.c\n\
         cc -O2  -c util.c\n\
         cc  init.o util.o -lcrypt -o init\n\
         cc -O2  -o killall5 killall5.c\n");
}

// sysvinit: `$(wildcard $(ROOT)/$(usrdir)/lib*/libcrypt.*)`, a wildcard in a
// directory component; a name starting with `.` needs a `.` to match.
#[test]
fn wildcard_in_a_directory_component() {
    let dir = Dir::new("up_wildcard_dirs");
    dir.write("lib64/libcrypt.so", "");
    dir.write("lib/x86/libcrypt.a", "");
    dir.write("libz/other", "");
    dir.write(".hidden/libcrypt.so", "");
    dir.write(
        "Makefile",
        "all:\n\t@echo [$(wildcard lib*/libcrypt.*)] [$(wildcard lib/*/libcrypt.*)] [$(wildcard */libcrypt.so)]\n",
    );
    dir.make(&[])
        .ok("[lib64/libcrypt.so] [lib/x86/libcrypt.a] [lib64/libcrypt.so]\n");
}

// libzstd's tests/Makefile: `LIB_SRCDIR` ends in `/`, so
// `$(wildcard $(LIB_SRCDIR)/decompress/*_amd64.S)` has a `//`, and the names
// found must keep it, as glob(3) and GNU make keep it, for
// `$(subst $(LIB_SRCDIR)/decompress/,zstdmt_d_,...)` to turn them into the
// object names its `zstdmt_d_%.o : $(LIB_SRCDIR)/decompress/%.S` makes.
#[test]
fn wildcard_names_are_spelled_as_the_pattern_spells_them() {
    let dir = Dir::new("up_wildcard_slashes");
    dir.write("lib/decompress/huf_amd64.S", "");
    dir.write(
        "Makefile",
        "SRC := lib/\nF := $(wildcard $(SRC)/decompress/*_amd64.S)\n\
         O := $(subst $(SRC)/decompress/,zstdmt_d_,$(F:.S=.o))\n\
         all:\n\t@echo [$(F)] [$(O)] [$(wildcard ./lib//*/*.S)] [$(wildcard */)]\n",
    );
    dir.make(&[])
        .ok("[lib//decompress/huf_amd64.S] [zstdmt_d_huf_amd64.o] \
         [./lib//decompress/huf_amd64.S] [lib/]\n");
}

// A command-line macro's value starts after the blanks that follow `=`, as a
// makefile's does: libzstd hands a sub-make `LDFLAGS="$(LDFLAGS)"`.
#[test]
fn a_command_line_value_loses_its_leading_blanks() {
    let dir = Dir::new("up_cmdline_blanks");
    dir.write(
        "Makefile",
        "Y += b\nall:\n\t@echo \"[$(X)] [$(Y)] [$$X]\"\n",
    );
    dir.make(&["X=  a ", "Y= c"]).ok("[a ] [c] [a ]\n");
}

// libzstd.mk: `LIB_SRCDIR ?= $(dir $(realpath $(lastword $(MAKEFILE_LIST))))`.
#[test]
fn makefile_list_and_realpath() {
    let dir = Dir::new("up_makefile_list");
    // `makefile`, the first name make looks for: on a case-insensitive file
    // system (macOS) a `Makefile` is found under that first name too, and
    // MAKEFILE_LIST records the name used, as in GNU make.
    dir.write(
        "makefile",
        "include sub/inc.mk\nall:\n\t@echo [$(notdir $(MAKEFILE_LIST))] [$(SRCDIR)]\n\t@echo [$(realpath . nosuch)]\n",
    );
    dir.write(
        "sub/inc.mk",
        "SRCDIR := $(notdir $(patsubst %/,%,$(dir $(realpath $(lastword $(MAKEFILE_LIST))))))\n",
    );
    let here = std::fs::canonicalize(dir.path(""))
        .unwrap()
        .to_string_lossy()
        .into_owned();
    dir.make(&[])
        .ok(&format!("[makefile inc.mk] [sub]\n[{here}]\n"));
}
