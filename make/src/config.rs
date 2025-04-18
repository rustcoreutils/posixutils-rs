//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::{BTreeMap, BTreeSet};

/// Represents the configuration of the make utility
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    /// Whether to ignore the errors in the rule
    pub ignore: bool,
    /// Whether to execute commands or print to stdout
    pub dry_run: bool,
    /// Whether to print recipe lines
    pub silent: bool,
    /// Whether to touch targets on execution
    pub touch: bool,
    /// Whether to replace macros within makefiles with envs
    pub env_macros: bool,
    /// Whether to quit without build
    pub quit: bool,
    /// Whether to keep going build targets and write info about errors stderr
    pub keep_going: bool,
    /// Whether to terminate on error
    pub terminate: bool,
    /// Whether to clear default_rules
    pub clear: bool,
    /// Whether to print macro definitions and target descriptions.
    pub print: bool,
    /// Whether to not delete interrupted files on async events.
    pub precious: bool,

    pub rules: BTreeMap<String, BTreeSet<String>>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            ignore: false,
            dry_run: false,
            silent: false,
            touch: false,
            env_macros: false,
            keep_going: false,
            quit: false,
            clear: false,
            print: false,
            precious: false,
            terminate: true,
            rules: BTreeMap::from([
                (
                    ".SUFFIXES".to_string(),
                    vec![
                        ".o", ".c", ".y", ".l", ".a", ".sh", ".c~", ".y~", ".l~", ".sh~",
                    ]
                    .into_iter()
                    .map(String::from)
                    .collect(),
                ),
                (
                    ".SCCS_GET".to_string(),
                    BTreeSet::from([String::from("sccs $(SCCSFLAGS) get $(SCCSGETFLAGS) $@")]),
                ),
                (
                    ".MACROS".to_string(),
                    vec![
                        "AR=ar",
                        "ARFLAGS=-rv",
                        "YACC=yacc",
                        "YFLAGS=",
                        "LEX=lex",
                        "LFLAGS=",
                        "LDFLAGS=",
                        "CC=c17",
                        "CFLAGS=-O 1",
                        "XSI GET=get",
                        "GFLAGS=",
                        "SCCSFLAGS=",
                        "SCCSGETFLAGS=-s",
                    ]
                    .into_iter()
                    .map(String::from)
                    .collect(),
                ),
                (
                    "SUFFIX RULES".to_string(),
                    [
                        // Single-Suffix Rules
                        ".c: $(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<",
                        ".sh: cp $< $@",
                        ".sh: chmod a+x $@",

                        // Double-Suffix Rules
                        ".c.o: $(CC) $(CFLAGS) -c $<",
                        ".y.o: $(YACC) $(YFLAGS) $<; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@",
                        ".l.o: $(LEX) $(LFLAGS) $<; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@",
                        ".y.c: $(YACC) $(YFLAGS) $<; mv y.tab.c $@",
                        ".l.c: $(LEX) $(LFLAGS) $<; mv lex.yy.c $@",
                        "XSI .c~.o: $(GET) $(GFLAGS) -p $< > $*.c; $(CC) $(CFLAGS) -c $*.c",
                        ".y~.o: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; $(CC) $(CFLAGS) -c y.tab.c; rm -f y.tab.c; mv y.tab.o $@",
                        ".l~.o: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; $(CC) $(CFLAGS) -c lex.yy.c; rm -f lex.yy.c; mv lex.yy.o $@",
                        ".y~.c: $(GET) $(GFLAGS) -p $< > $*.y; $(YACC) $(YFLAGS) $*.y; mv y.tab.c $@",
                        ".l~.c: $(GET) $(GFLAGS) -p $< > $*.l; $(LEX) $(LFLAGS) $*.l; mv lex.yy.c $@",
                        ".c.a: $(CC) -c $(CFLAGS) $<; $(AR) $(ARFLAGS) $@ $*.o; rm -f $*.o",
                    ]
                    .into_iter()
                    .map(String::from)
                    .collect::<BTreeSet<String>>(),
            )
            ]),
        }
    }
}

impl Config {
    /// Adds a new suffix to the `.SUFFIXES` rule.
    pub fn add_suffix(&mut self, new_suffix: &str) {
        self.rules
            .entry(".SUFFIXES".to_string())
            .or_default()
            .insert(new_suffix.to_string());
    }
}
