// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod preprocess {
    use posixutils_make::parser::preprocessor::preprocess;

    #[test]
    fn test_macros_simple() {
        const MACROS: &str = r#"
VAR = var
V = ok

all:
	$(VAR) $V ${VAR} ${V} $(V)
"#;

        // Two macro definitions are blanked rather than deleted, so each
        // leaves an empty line behind and every later line keeps its original
        // number. The original blank line is still there too, hence four.
        const EXPECTED: &str = r#"



all:
	var ok var ok ok
"#;
        let Ok((result, _macros)) = preprocess(MACROS) else {
            panic!("Test must be preprocessed without an error")
        };
        assert_eq!(result, EXPECTED);
    }

    // Audit #6: `$(VAR:subst1=subst2)` suffix substitution.
    #[test]
    fn test_subst_suffix() {
        let result = preprocess("SRC = a.c b.c foo.c\nall:\n\t@echo $(SRC:.c=.o)\n")
            .unwrap()
            .0;
        assert!(result.contains("@echo a.o b.o foo.o"), "got: {result:?}");
    }

    // Audit #6: `$(VAR:op%os=np%ns)` pattern substitution.
    #[test]
    fn test_subst_pattern() {
        let result = preprocess("O = a.o b.o\nall:\n\t@echo $(O:%.o=%.x)\n")
            .unwrap()
            .0;
        assert!(result.contains("@echo a.x b.x"), "got: {result:?}");
    }

    // Audit #7: backslash-newline continuation is folded to a space in a
    // macro definition.
    #[test]
    fn test_continuation_macro() {
        let result = preprocess("FOO = a\\\nb\nall:\n\t@echo $(FOO)\n")
            .unwrap()
            .0;
        assert!(result.contains("@echo a b"), "got: {result:?}");
    }

    // Audit #7: backslash-newline continuation in a recipe line is spliced
    // (the leading tab of the continuation is removed).
    #[test]
    fn test_continuation_recipe() {
        let result = preprocess("all:\n\t@echo one \\\n\ttwo\n").unwrap().0;
        assert!(result.contains("@echo one two"), "got: {result:?}");
    }

    // Audit #15: internal-macro references survive preprocessing for the
    // rule stage rather than being expanded or rejected here.
    #[test]
    fn test_internal_macros_passthrough() {
        let result = preprocess("all: a b\n\t@echo $^ $+ $(@D) $(@F) ${?F}\n")
            .unwrap()
            .0;
        assert!(
            result.contains("@echo $^ $+ $(@D) $(@F) ${?F}"),
            "got: {result:?}"
        );
    }

    // Audit #19: a missing `-include` file is ignored (no error); a missing
    // plain `include` is an error.
    #[test]
    fn test_dash_include_missing_ignored() {
        let result = preprocess("-include /nonexistent_xyz.mk\nall:\n\t@echo ok\n");
        assert!(result.is_ok(), "got: {result:?}");
        assert!(result.unwrap().0.contains("@echo ok"));
    }

    #[test]
    fn test_include_missing_errors() {
        let result = preprocess("include /nonexistent_xyz.mk\nall:\n\t@echo ok\n");
        assert!(result.is_err());
    }

    // Audit #19: `includedir = ...` is a macro definition, not an include
    // directive (it lacks the required trailing blank after `include`).
    #[test]
    fn test_includedir_not_mistaken_for_include() {
        let result = preprocess("includedir = /usr\nall:\n\t@echo $(includedir)\n")
            .unwrap()
            .0;
        assert!(result.contains("@echo /usr"), "got: {result:?}");
    }

    // Review (c2): a substitution error in an include path is propagated, not
    // swallowed into a misleading empty path.
    #[test]
    fn test_include_path_substitution_error_propagates() {
        let result = preprocess("include $(UNDEF)/x.mk\nall:\n\t@echo hi\n");
        assert!(result.is_err(), "expected an error, got: {result:?}");
    }

    // Review (c3): a `$(name:subst=...)` form missing its closing delimiter is a
    // clear error rather than silently consuming to EOF.
    #[test]
    fn test_subst_missing_close_errors() {
        let result = preprocess("V = a.c\nall:\n\t@echo $(V:.c=.o\n");
        assert!(result.is_err(), "expected an error, got: {result:?}");
    }

    fn macros_of(source: &str) -> Vec<(String, String)> {
        preprocess(source).expect("must preprocess").1
    }

    fn value_of(source: &str, name: &str) -> String {
        macros_of(source)
            .into_iter()
            .find(|(n, _)| n == name)
            .unwrap_or_else(|| panic!("macro {name} not returned"))
            .1
    }

    // Audit #36: macro definitions must reach the caller. They used to be
    // consumed here and dropped, leaving `Make::macros` permanently empty and
    // the `SHELL` macro inert.
    #[test]
    fn test_macros_are_returned() {
        assert_eq!(
            value_of("SHELL = /bin/bash\nall:\n\techo hi\n", "SHELL"),
            "/bin/bash"
        );
    }

    // The returned table keeps first-definition order, so the macros handed to
    // `Make` are deterministic.
    #[test]
    fn test_macro_order_is_first_definition() {
        let names: Vec<String> = macros_of("B = 2\nA = 1\nB = 3\nall:\n\techo hi\n")
            .into_iter()
            .map(|(n, _)| n)
            .collect();
        assert_eq!(names, vec!["B".to_string(), "A".to_string()]);
    }

    // POSIX assignment operators, checked through the returned table rather
    // than through their effect on the text.
    #[test]
    fn test_append_operator() {
        assert_eq!(value_of("A = 1\nA += 2\nall:\n\techo hi\n", "A"), "1 2");
    }

    #[test]
    fn test_append_to_undefined_is_plain_assignment() {
        assert_eq!(value_of("A += 2\nall:\n\techo hi\n", "A"), "2");
    }

    #[test]
    fn test_conditional_operator_keeps_existing() {
        assert_eq!(value_of("A = y\nA ?= x\nall:\n\techo hi\n", "A"), "y");
    }

    #[test]
    fn test_conditional_operator_assigns_when_unset() {
        assert_eq!(value_of("A ?= x\nall:\n\techo hi\n", "A"), "x");
    }

    // POSIX 105746-105748: `!=` strips leading white space, drops one trailing
    // <newline>, and turns every remaining <newline> into a <space>.
    #[test]
    fn test_shell_assignment_whitespace_rules() {
        assert_eq!(
            value_of("A != printf 'one\\ntwo\\n'\nall:\n\techo hi\n", "A"),
            "one two"
        );
    }

    // Audit #35: an include path may reference a macro. The include pass used
    // to be handed an empty table, so this was always UndefinedMacro.
    #[test]
    fn test_include_path_may_use_a_macro() {
        let dir = std::env::temp_dir().join("make_preproc_include_macro");
        std::fs::create_dir_all(&dir).unwrap();
        let inc = dir.join("inc.mk");
        std::fs::write(&inc, "included:\n\t@echo from-include\n").unwrap();

        let source = format!("TOP = {}\ninclude $(TOP)/inc.mk\n", dir.display());
        let text = preprocess(&source)
            .expect("include with a macro in its path")
            .0;
        assert!(text.contains("from-include"), "got: {text:?}");

        std::fs::remove_dir_all(&dir).ok();
    }
}

// `mod lex` used to assert token streams for the hand-written lexer. That
// lexer is gone: the scanner replaced it, and its lexical helpers are unit
// tested inside `src/parser/scan.rs`, where they can stay private.

mod parse {
    use posixutils_make::parser::parse::parse;
    use posixutils_make::parser::Makefile;
    use std::str::FromStr;

    fn parsed(source: &str) -> Makefile {
        Makefile::from_str(source).expect("must parse")
    }

    fn only_rule(source: &str) -> (Vec<String>, Vec<String>, Vec<String>) {
        let mk = parsed(source);
        let rule = mk.rules().next().expect("one rule").clone();
        (
            rule.targets().map(String::from).collect(),
            rule.prerequisites().map(String::from).collect(),
            rule.recipes().map(String::from).collect(),
        )
    }

    #[test]
    fn test_parse_simple() {
        const SIMPLE: &str =
            "VARIABLE = command2\n\nrule: dependency\n\tcommand\n\t${VARIABLE}\n\n";
        let (targets, prerequisites, recipes) = only_rule(SIMPLE);
        assert_eq!(targets, vec!["rule"]);
        assert_eq!(prerequisites, vec!["dependency"]);
        assert_eq!(recipes, vec!["command", "command2"]);
    }

    #[test]
    fn test_parse_multiple_prerequisites() {
        let (targets, prerequisites, recipes) = only_rule("rule: a b c\n\techo hi\n");
        assert_eq!(targets, vec!["rule"]);
        assert_eq!(prerequisites, vec!["a", "b", "c"]);
        assert_eq!(recipes, vec!["echo hi"]);
    }

    // Audit #27: a rule may name more than one target. This was a hard parse
    // error ("expected ':'") because the parser consumed exactly one.
    #[test]
    fn test_multiple_targets_per_rule() {
        let (targets, prerequisites, _) = only_rule("a b: dep\n\techo hi\n");
        assert_eq!(targets, vec!["a", "b"]);
        assert_eq!(prerequisites, vec!["dep"]);
    }

    // Audit #26: `/` is an ordinary name character. It used to lex as an ERROR
    // token, and `prerequisites()` dropped everything that was not an
    // IDENTIFIER, so `src/foo.c` silently became `src` and `foo.c`.
    #[test]
    fn test_slash_in_prerequisite() {
        let (_, prerequisites, _) = only_rule("all: src/foo.c\n\techo hi\n");
        assert_eq!(prerequisites, vec!["src/foo.c"]);
    }

    #[test]
    fn test_slash_in_target() {
        let (targets, _, _) = only_rule("build/x.o: x.c\n\techo hi\n");
        assert_eq!(targets, vec!["build/x.o"]);
    }

    // POSIX 105946: parentheses mean an archive member, and the name must
    // survive whole for the archive-member mtime lookup.
    #[test]
    fn test_archive_member_target() {
        let (targets, _, _) = only_rule("lib.a(mem.o): mem.c\n\techo hi\n");
        assert_eq!(targets, vec!["lib.a(mem.o)"]);
    }

    // POSIX 105644: text after a <semicolon> is a command line.
    #[test]
    fn test_inline_command() {
        let (_, prerequisites, recipes) = only_rule("rule: dep ; echo inline\n");
        assert_eq!(prerequisites, vec!["dep"]);
        assert_eq!(recipes, vec!["echo inline"]);
    }

    // POSIX 105911-105915: `target: ;` is the empty rule -- it has a command,
    // and that command is empty. That distinction decides whether an inference
    // rule is consulted for the target.
    #[test]
    fn test_empty_rule_has_a_command() {
        let (_, _, recipes) = only_rule("rule: ;\n");
        assert_eq!(recipes, vec![""]);
    }

    #[test]
    fn test_comment_after_a_rule_header() {
        let (targets, prerequisites, _) = only_rule("comment: # this is a comment\n\techo hi\n");
        assert_eq!(targets, vec!["comment"]);
        assert!(prerequisites.is_empty());
    }

    // POSIX 105629: a command line reaches the shell verbatim, so `#` is not a
    // comment there. This used to be a hard parse error.
    #[test]
    fn test_hash_survives_in_a_command() {
        let (_, _, recipes) = only_rule("all:\n\t@echo \"#!/bin/sh\"\n");
        assert_eq!(recipes, vec!["@echo \"#!/bin/sh\""]);
    }

    // POSIX 105646: only a non-empty line that does not begin with <tab> or
    // `#` begins a new entry, so a blank line inside a recipe does not end it.
    #[test]
    fn test_blank_line_does_not_end_a_recipe() {
        let (_, _, recipes) = only_rule("all:\n\tcmd1\n\n\tcmd2\n");
        assert_eq!(recipes, vec!["cmd1", "cmd2"]);
    }

    #[test]
    fn test_tab_only_line_is_ignored() {
        let (_, _, recipes) = only_rule("all:\n\t\n\tcmd\n");
        assert_eq!(recipes, vec!["cmd"]);
    }

    // Audit #36: the macros the preprocessor consumed reach the caller.
    #[test]
    fn test_macros_reach_the_makefile() {
        let mk = parsed("SHELL = /bin/bash\nall:\n\techo hi\n");
        assert_eq!(
            mk.macros()
                .iter()
                .find(|(n, _)| n == "SHELL")
                .map(|(_, v)| v.as_str()),
            Some("/bin/bash")
        );
    }

    // Replaces the old add_rule / push_command / replace_command tests, which
    // existed only to exercise rowan tree mutation and had no in-crate caller.
    #[test]
    fn test_display_round_trip() {
        let mk = parsed("a b: c\n\techo hi\n");
        assert_eq!(mk.to_string(), "a b: c\n\techo hi\n");
    }

    #[test]
    fn test_no_targets_is_an_error() {
        assert!(Makefile::from_str("MACRO = value\n").is_err());
    }

    #[test]
    fn test_semicolon_before_colon_is_an_error() {
        assert!(parse("a;b: c\n").is_err());
    }

    #[test]
    fn test_command_before_any_target_is_an_error() {
        assert!(parse("\techo orphan\n").is_err());
    }
}
