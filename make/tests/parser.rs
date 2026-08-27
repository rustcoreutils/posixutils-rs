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
        let Ok(scanned) = preprocess(MACROS) else {
            panic!("Test must be preprocessed without an error")
        };
        assert_eq!(scanned.text, EXPECTED);
    }

    // Audit #6: `$(VAR:subst1=subst2)` suffix substitution.
    #[test]
    fn test_subst_suffix() {
        let result = preprocess("SRC = a.c b.c foo.c\nall:\n\t@echo $(SRC:.c=.o)\n")
            .unwrap()
            .text;
        assert!(result.contains("@echo a.o b.o foo.o"), "got: {result:?}");
    }

    // Audit #6: `$(VAR:op%os=np%ns)` pattern substitution.
    #[test]
    fn test_subst_pattern() {
        let result = preprocess("O = a.o b.o\nall:\n\t@echo $(O:%.o=%.x)\n")
            .unwrap()
            .text;
        assert!(result.contains("@echo a.x b.x"), "got: {result:?}");
    }

    // Audit #7: backslash-newline continuation is folded to a space in a
    // macro definition.
    #[test]
    fn test_continuation_macro() {
        let result = preprocess("FOO = a\\\nb\nall:\n\t@echo $(FOO)\n")
            .unwrap()
            .text;
        assert!(result.contains("@echo a b"), "got: {result:?}");
    }

    // Audit #7: backslash-newline continuation in a recipe line is spliced
    // (the leading tab of the continuation is removed).
    #[test]
    fn test_continuation_recipe() {
        let result = preprocess("all:\n\t@echo one \\\n\ttwo\n").unwrap().text;
        assert!(result.contains("@echo one two"), "got: {result:?}");
    }

    // Audit #15: internal-macro references survive preprocessing for the
    // rule stage rather than being expanded or rejected here.
    #[test]
    fn test_internal_macros_passthrough() {
        let result = preprocess("all: a b\n\t@echo $^ $+ $(@D) $(@F) ${?F}\n")
            .unwrap()
            .text;
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
        assert!(result.unwrap().text.contains("@echo ok"));
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
            .text;
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
        preprocess(source).expect("must preprocess").macros
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
            .text;
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

// Conditionals and multi-line macro definitions. Not POSIX -- the standard has
// no conditionals -- but 357 occurrences across a seven-Makefile sample of real
// projects, far and away the most common construct a POSIX-only make cannot
// read.
mod conditionals {
    use posixutils_make::parser::preprocessor::preprocess;

    fn text(source: &str) -> String {
        preprocess(source).expect("must preprocess").text
    }

    fn value(source: &str, name: &str) -> String {
        preprocess(source)
            .expect("must preprocess")
            .macros
            .into_iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v)
            .unwrap_or_default()
    }

    #[test]
    fn ifeq_selects_the_matching_arm() {
        let src =
            "OS = Linux\nifeq ($(OS),Linux)\nCC = gcc\nelse\nCC = clang\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "CC"), "gcc");
    }

    #[test]
    fn ifeq_takes_the_else_arm_when_it_does_not_match() {
        let src =
            "OS = Darwin\nifeq ($(OS),Linux)\nCC = gcc\nelse\nCC = clang\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "CC"), "clang");
    }

    #[test]
    fn ifneq_inverts_the_test() {
        let src = "A = x\nifneq ($(A),y)\nR = ok\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "R"), "ok");
    }

    // The canonical "is it set?" idiom. It needs an undefined macro to expand
    // to the empty string rather than error (POSIX 105833).
    #[test]
    fn comparing_an_undefined_macro_to_empty_works() {
        let src = "ifeq ($(UNSET),)\nR = empty\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "R"), "empty");
    }

    #[test]
    fn ifdef_and_ifndef() {
        let src = "A = 1\nifdef A\nR = yes\nendif\nifndef B\nS = alsoyes\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "R"), "yes");
        assert_eq!(value(src, "S"), "alsoyes");
    }

    #[test]
    fn else_if_chains() {
        let src = "V = 2\nifeq ($(V),1)\nR = one\nelse ifeq ($(V),2)\nR = two\nelse\nR = other\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "R"), "two");
    }

    #[test]
    fn conditionals_nest_and_may_be_space_indented() {
        let src =
            "V = 2\nifeq ($(V),2)\n  ifdef V\n  N = nested\n  endif\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "N"), "nested");
    }

    #[test]
    fn quoted_argument_form_is_accepted() {
        let src = "A = x\nifeq \"$(A)\" \"x\"\nQ = ok\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "Q"), "ok");
    }

    // A condition inside a dead branch must not be evaluated -- it may name
    // macros only the live arm defines.
    #[test]
    fn an_inactive_branch_hides_its_contents() {
        let src = "ifeq (a,b)\nR = taken\nendif\nall:\n\techo hi\n";
        assert_eq!(value(src, "R"), "");
    }

    // A guarded include must not be read when its branch is dead.
    #[test]
    fn an_inactive_branch_does_not_read_an_include() {
        let src = "ifeq (a,b)\ninclude /nonexistent_guarded_xyz.mk\nendif\nall:\n\techo hi\n";
        assert!(preprocess(src).is_ok(), "guarded include must not be read");
    }

    #[test]
    fn unmatched_endif_is_an_error() {
        assert!(preprocess("all:\n\techo hi\nendif\n").is_err());
    }

    #[test]
    fn unterminated_conditional_is_an_error() {
        assert!(preprocess("ifeq (a,a)\nall:\n\techo hi\n").is_err());
    }

    #[test]
    fn define_captures_a_multi_line_body() {
        let src = "define B\none\ntwo\nendef\nall:\n\techo hi\n";
        assert_eq!(value(src, "B"), "one\ntwo");
    }

    // A multi-line value used in a recipe must stay a recipe: every embedded
    // newline gets the command line's <tab>.
    #[test]
    fn define_used_in_a_recipe_stays_indented() {
        let src = "define B\n@echo one\n@echo two\nendef\nall:\n\t$(B)\n";
        let out = text(src);
        assert!(out.contains("\t@echo one\n\t@echo two"), "got: {out:?}");
    }

    // Audit #32: a self-including file used to loop forever.
    #[test]
    fn include_recursion_is_capped() {
        let dir = std::env::temp_dir().join("make_include_cycle_test");
        std::fs::create_dir_all(&dir).unwrap();
        let f = dir.join("self.mk");
        std::fs::write(&f, format!("include {}\n", f.display())).unwrap();
        let src = format!("include {}\nall:\n\techo hi\n", f.display());
        assert!(preprocess(&src).is_err(), "self-include must not loop");
        std::fs::remove_dir_all(&dir).ok();
    }

    // Audit #32: `A = $(A)x` used to grow the text forever.
    #[test]
    fn recursive_macro_is_capped() {
        assert!(preprocess("A = $(A)x\nall:\n\techo $(A)\n").is_err());
    }

    // Audit #34: an undefined macro is the empty string, not a fatal error.
    #[test]
    fn undefined_macro_expands_to_empty() {
        let out = text("all:\n\t@echo [$(UNSET)]\n");
        assert!(out.contains("@echo []"), "got: {out:?}");
    }

    // Audit #34: the environment is macro source 3 unconditionally; `-e` only
    // changes which source wins.
    #[test]
    fn environment_is_a_macro_source_without_dash_e() {
        std::env::set_var("MAKE_PARSER_ENV_PROBE", "fromenv");
        let out = text("all:\n\t@echo [$(MAKE_PARSER_ENV_PROBE)]\n");
        std::env::remove_var("MAKE_PARSER_ENV_PROBE");
        assert!(out.contains("@echo [fromenv]"), "got: {out:?}");
    }

    // Audit #34: substitution used to run over comment text, so a bare `$`
    // in a comment aborted the whole parse.
    #[test]
    fn a_dollar_in_a_comment_does_not_abort() {
        assert!(preprocess("# price is $5\nall:\n\techo ok\n").is_ok());
    }
}

// Macro functions. Not POSIX; the set was chosen by counting occurrences
// across real Makefiles rather than by mirroring GNU's roster.
mod functions {
    use posixutils_make::parser::preprocessor::preprocess;

    fn echoed(body: &str) -> String {
        let src = format!("all:\n\t@echo [{body}]\n");
        let out = preprocess(&src).expect("must preprocess").text;
        let start = out.find('[').expect("marker");
        let end = out.rfind(']').expect("marker");
        out[start + 1..end].to_string()
    }

    #[test]
    fn text_functions() {
        assert_eq!(echoed("$(subst ee,EE,feet street)"), "fEEt strEEt");
        assert_eq!(echoed("$(patsubst %.c,%.o,a.c b.c)"), "a.o b.o");
        assert_eq!(echoed("$(strip   a   b  )"), "a b");
        assert_eq!(echoed("$(sort b a c a)"), "a b c");
        assert_eq!(echoed("$(filter %.c,a.c b.o)"), "a.c");
        assert_eq!(echoed("$(filter-out %.c,a.c b.o)"), "b.o");
        assert_eq!(echoed("$(findstring a,a b)"), "a");
    }

    #[test]
    fn path_functions() {
        assert_eq!(echoed("$(dir src/foo.c)"), "src/");
        assert_eq!(echoed("$(notdir src/foo.c)"), "foo.c");
        assert_eq!(echoed("$(basename src/foo.c)"), "src/foo");
        assert_eq!(echoed("$(suffix src/foo.c)"), ".c");
        assert_eq!(echoed("$(addprefix obj/,a.o b.o)"), "obj/a.o obj/b.o");
        assert_eq!(echoed("$(addsuffix .o,a b)"), "a.o b.o");
    }

    #[test]
    fn word_functions() {
        assert_eq!(echoed("$(words a b c)"), "3");
        assert_eq!(echoed("$(word 2,a b c)"), "b");
        assert_eq!(echoed("$(wordlist 2,3,a b c d)"), "b c");
        assert_eq!(echoed("$(firstword a b)"), "a");
        assert_eq!(echoed("$(lastword a b)"), "b");
    }

    #[test]
    fn shell_function() {
        assert_eq!(echoed("$(shell echo hello)"), "hello");
    }

    #[test]
    fn conditional_functions() {
        assert_eq!(echoed("$(if ,yes,no)"), "no");
        assert_eq!(echoed("$(if x,yes,no)"), "yes");
        assert_eq!(echoed("$(or ,,third)"), "third");
        assert_eq!(echoed("$(and a,b)"), "b");
        assert_eq!(echoed("$(and a,,c)"), "");
    }

    #[test]
    fn foreach_binds_its_variable() {
        assert_eq!(echoed("$(foreach v,1 2 3,[$(v)])"), "[1] [2] [3]");
    }

    #[test]
    fn call_binds_positional_arguments() {
        let src = "greet = hi $(1) and $(2)\nall:\n\t@echo [$(call greet,a,b)]\n";
        let out = preprocess(src).expect("must preprocess").text;
        assert!(out.contains("[hi a and b]"), "got: {out:?}");
    }

    // A self-referential $(call) recursed through
    // substitute -> func::call -> expand -> substitute until the stack was
    // exhausted: "fatal runtime error: stack overflow", core dumped.
    // MAX_EXPANSION_ROUNDS bounds the rounds within one frame, not the depth of
    // that cycle, so a separate depth guard was needed.
    #[test]
    fn self_referential_call_is_capped_not_a_crash() {
        let err = preprocess("A = $(call A)\nall:\n\t@echo $(A)\n");
        assert!(err.is_err(), "expected a depth error");
    }

    #[test]
    fn mutually_recursive_calls_are_capped() {
        let err = preprocess("A = $(call B)\nB = $(call A)\nall:\n\t@echo $(A)\n");
        assert!(err.is_err(), "expected a depth error");
    }

    #[test]
    fn recursion_through_foreach_is_capped() {
        let err = preprocess("A = $(foreach x,1,$(call A))\nall:\n\t@echo $(A)\n");
        assert!(err.is_err(), "expected a depth error");
    }

    // The cap must not reject legitimate nesting.
    #[test]
    fn finite_nesting_still_expands() {
        let src = "f = [$(1)]\nall:\n\t@echo $(call f,$(call f,$(call f,x)))\n";
        let out = preprocess(src).expect("must preprocess").text;
        assert!(out.contains("[[[x]]]"), "got: {out:?}");
    }

    // The depth error crosses the func/preprocessor String boundary once per
    // level; it must not accumulate a newline each time.
    #[test]
    fn a_depth_error_is_reported_once() {
        let err = preprocess("A = $(call A)\nall:\n\t@echo $(A)\n")
            .expect_err("must error")
            .to_string();
        assert_eq!(err.lines().count(), 1, "got: {err:?}");
    }

    #[test]
    fn functions_nest() {
        assert_eq!(echoed("$(sort $(patsubst %.c,%.o,b.c a.c))"), "a.o b.o");
    }

    // A comma inside a nested reference must not split the argument list.
    #[test]
    fn nested_commas_do_not_split_arguments() {
        assert_eq!(echoed("$(words $(subst a,b,a a a))"), "3");
    }

    #[test]
    fn wildcard_lists_matching_files() {
        let dir = std::env::temp_dir().join("make_wildcard_probe");
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("one.c"), "").unwrap();
        std::fs::write(dir.join("two.c"), "").unwrap();
        std::fs::write(dir.join("skip.h"), "").unwrap();
        let src = format!("all:\n\t@echo [$(wildcard {}/*.c)]\n", dir.display());
        let out = preprocess(&src).expect("must preprocess").text;
        assert!(out.contains("one.c"), "got: {out:?}");
        assert!(out.contains("two.c"), "got: {out:?}");
        assert!(!out.contains("skip.h"), "got: {out:?}");
        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn error_function_stops_the_parse() {
        assert!(preprocess("all:\n\t@echo $(error boom)\n").is_err());
    }

    // A recipe is expanded after the reader has finished, so text queued there
    // would have no consumer. Refused rather than silently dropped.
    #[test]
    fn eval_in_a_recipe_is_refused() {
        assert!(preprocess("all:\n\t@echo $(eval X = 1)\n").is_err());
    }

    // An unimplemented function is refused rather than silently expanding to
    // nothing, so a makefile using one fails loudly instead of building wrong.
    #[test]
    fn an_unknown_function_name_is_refused() {
        assert!(preprocess("all:\n\t@echo $(nosuchthing foo)\n").is_err());
    }
}

// The `vpath` directive: per-pattern prerequisite search paths.
mod vpath {
    use posixutils_make::parser::preprocessor::preprocess;

    fn entries(source: &str) -> Vec<(String, Vec<String>)> {
        preprocess(source)
            .expect("must preprocess")
            .vpaths
            .into_iter()
            .map(|e| (e.pattern, e.dirs))
            .collect()
    }

    #[test]
    fn add_records_a_pattern_and_its_directories() {
        let got = entries("vpath %.c src:lib\nall:\n\techo hi\n");
        assert_eq!(
            got,
            vec![(
                "%.c".to_string(),
                vec!["src".to_string(), "lib".to_string()]
            )]
        );
    }

    #[test]
    fn blank_separated_directories_are_accepted_too() {
        let got = entries("vpath %.c src lib\nall:\n\techo hi\n");
        assert_eq!(got[0].1, vec!["src".to_string(), "lib".to_string()]);
    }

    // Repeating the directive for a pattern appends, as GNU does.
    #[test]
    fn repeating_a_pattern_appends() {
        let got = entries("vpath %.c src\nvpath %.c lib\nall:\n\techo hi\n");
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].1, vec!["src".to_string(), "lib".to_string()]);
    }

    #[test]
    fn several_patterns_are_kept_in_order() {
        let got = entries("vpath %.c src\nvpath %.h hdr\nall:\n\techo hi\n");
        assert_eq!(got.len(), 2);
        assert_eq!(got[0].0, "%.c");
        assert_eq!(got[1].0, "%.h");
    }

    #[test]
    fn pattern_alone_clears_that_pattern() {
        let got = entries("vpath %.c src\nvpath %.h hdr\nvpath %.c\nall:\n\techo hi\n");
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].0, "%.h");
    }

    #[test]
    fn bare_vpath_clears_everything() {
        let got = entries("vpath %.c src\nvpath %.h hdr\nvpath\nall:\n\techo hi\n");
        assert!(got.is_empty());
    }

    #[test]
    fn the_argument_is_macro_expanded() {
        let got = entries("D = src\nvpath %.c $(D)\nall:\n\techo hi\n");
        assert_eq!(got[0].1, vec!["src".to_string()]);
    }

    // A dead conditional branch must not record its vpath.
    #[test]
    fn an_inactive_branch_records_nothing() {
        let got = entries("ifeq (a,b)\nvpath %.c src\nendif\nall:\n\techo hi\n");
        assert!(got.is_empty());
    }

    // `vpathological:` is a target, not the directive.
    #[test]
    fn a_longer_word_is_not_the_directive() {
        let got = entries("vpathological: dep\n\techo hi\n");
        assert!(got.is_empty());
    }
}

// `$(eval ...)`: expand text, then read it back as makefile source.
mod eval {
    use posixutils_make::parser::preprocessor::preprocess;
    use posixutils_make::parser::Makefile;
    use std::str::FromStr;

    fn text(source: &str) -> String {
        preprocess(source).expect("must preprocess").text
    }

    fn macro_value(source: &str, name: &str) -> String {
        preprocess(source)
            .expect("must preprocess")
            .macros
            .into_iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v)
            .unwrap_or_default()
    }

    fn rule_prereqs(source: &str, target: &str) -> Vec<String> {
        Makefile::from_str(source)
            .expect("must parse")
            .rules()
            .find(|r| r.targets().any(|t| t == target))
            .map(|r| r.prerequisites().map(String::from).collect())
            .unwrap_or_default()
    }

    #[test]
    fn defines_a_macro() {
        assert_eq!(macro_value("$(eval X = 1)\nall:\n\techo hi\n", "X"), "1");
    }

    #[test]
    fn defines_a_rule() {
        assert_eq!(
            rule_prereqs("$(eval gen: dep)\nall:\n\techo hi\n", "gen"),
            vec!["dep".to_string()]
        );
    }

    // The idiom eval exists for. Each iteration must queue separately, in order.
    #[test]
    fn generates_a_rule_per_foreach_iteration() {
        let src = "define tpl\n$(1): $(1).in\nendef\n                   $(foreach p,foo bar,$(eval $(call tpl,$(p))))\nall:\n\techo hi\n";
        assert_eq!(rule_prereqs(src, "foo"), vec!["foo.in".to_string()]);
        assert_eq!(rule_prereqs(src, "bar"), vec!["bar.in".to_string()]);
    }

    // A template writes `$$(CC)` so the *generated* recipe says `$(CC)`. Our
    // `substitute` passes `$$` through untouched, so eval consumes one level to
    // match GNU. Without it the recipe would reach the shell as a command
    // substitution and try to run `CC` as a program.
    #[test]
    fn consumes_one_dollar_level() {
        let src = "define tpl\n$(1).o: $(1).c\n\t@echo $$(CC)\nendef\n                   $(eval $(call tpl,foo))\nall:\n\techo hi\n";
        let out = text(src);
        assert!(
            !out.contains("$$(CC)"),
            "one level should be consumed: {out:?}"
        );
    }

    // A rule header from an eval must land *before* the line that produced it,
    // or it closes the enclosing rule and steals its recipe.
    #[test]
    fn a_generated_rule_does_not_steal_the_enclosing_recipe() {
        let src = "all: $(eval dep: ; @echo DEP)\n\t@echo ALL-RECIPE\n";
        let mk = Makefile::from_str(src).expect("must parse");
        let all = mk
            .rules()
            .find(|r| r.targets().any(|t| t == "all"))
            .expect("all");
        assert!(
            all.recipes().any(|r| r.contains("ALL-RECIPE")),
            "recipe attached to the wrong rule"
        );
    }

    // eval reachable from a conditional's condition, an include path, and a
    // macro body -- every path the reader expands, not just plain lines.
    #[test]
    fn works_inside_a_conditional_condition() {
        let src = "ifeq ($(eval X=1)x,x)\nY = taken\nendif\nall:\n\techo hi\n";
        assert_eq!(macro_value(src, "X"), "1");
        assert_eq!(macro_value(src, "Y"), "taken");
    }

    #[test]
    fn works_in_an_immediate_assignment() {
        let src = "Y := $(eval X = 1)\nall:\n\techo hi\n";
        assert_eq!(macro_value(src, "X"), "1");
        assert_eq!(macro_value(src, "Y"), "");
    }

    // Commas inside eval's text are literal: it takes one argument, and
    // splitting on them would cut a reference in half.
    #[test]
    fn commas_are_literal() {
        assert_eq!(
            rule_prereqs("$(eval t: a,b)\nall:\n\techo hi\n", "t"),
            vec!["a,b".to_string()]
        );
    }

    // eval'd text gets its own conditional stack, so a stray `endif` cannot
    // un-gate a conditional belonging to the enclosing file.
    #[test]
    fn an_eval_endif_cannot_close_an_outer_conditional() {
        let src = "ifeq (a,b)\n$(eval endif)\nGATED = leaked\nendif\nall:\n\techo hi\n";
        assert_eq!(macro_value(src, "GATED"), "");
    }

    #[test]
    fn an_unbalanced_conditional_inside_eval_is_refused() {
        assert!(preprocess("$(eval ifeq (1,1))\nall:\n\techo hi\n").is_err());
    }

    #[test]
    fn nested_eval_works() {
        assert_eq!(
            macro_value("$(eval $(eval X=1))\nall:\n\techo hi\n", "X"),
            "1"
        );
    }

    #[test]
    fn self_producing_eval_terminates() {
        assert!(preprocess("R = $(eval $(R))\nall:\n\t@echo $(R)\n").is_err());
    }

    #[test]
    fn evaluating_to_nothing_is_a_noop() {
        assert!(preprocess("$(eval )\nall:\n\techo hi\n").is_ok());
    }
}
