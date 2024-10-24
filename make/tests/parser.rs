// Copyright (c) 2024 Hemi Labs, Inc.
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
        const MACROS: &'static str = r#"
VAR = var
V = ok

all:
	$(VAR) $V ${VAR} ${V} $(V)
"#;

        const EXPECTED: &'static str = r#"

all:
	var ok var ok ok
"#;
        let Ok(result) = preprocess(MACROS) else {
            panic!("Test must be preprocessed without an error")
        };
        assert_eq!(result, EXPECTED);
    }
}

mod lex {
    use posixutils_make::parser::{lex::lex, SyntaxKind::*};

    #[test]
    fn test_empty() {
        assert_eq!(lex(""), vec![]);
    }

    #[test]
    fn test_simple() {
        assert_eq!(
            lex(r#"VARIABLE = value

rule: prerequisite
	recipe
"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            vec![
                (IDENTIFIER, "VARIABLE"),
                (WHITESPACE, " "),
                (EQUALS, "="),
                (WHITESPACE, " "),
                (IDENTIFIER, "value"),
                (NEWLINE, "\n"),
                (NEWLINE, "\n"),
                (IDENTIFIER, "rule"),
                (COLON, ":"),
                (WHITESPACE, " "),
                (IDENTIFIER, "prerequisite"),
                (NEWLINE, "\n"),
                (INDENT, "\t"),
                (TEXT, "recipe"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_bare_export() {
        assert_eq!(
            lex(r#"export
"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            vec![(EXPORT, "export"), (NEWLINE, "\n"),]
        );
    }

    #[test]
    fn test_export() {
        assert_eq!(
            lex(r#"export VARIABLE
"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            vec![
                (EXPORT, "export"),
                (WHITESPACE, " "),
                (IDENTIFIER, "VARIABLE"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_export_assignment() {
        assert_eq!(
            lex(r#"export VARIABLE := value
"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            vec![
                (EXPORT, "export"),
                (WHITESPACE, " "),
                (IDENTIFIER, "VARIABLE"),
                (WHITESPACE, " "),
                (COLON, ":"),
                (EQUALS, "="),
                (WHITESPACE, " "),
                (IDENTIFIER, "value"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_include() {
        assert_eq!(
            lex(r#"include FILENAME
"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            [
                (INCLUDE, "include"),
                (WHITESPACE, " "),
                (IDENTIFIER, "FILENAME"),
                (NEWLINE, "\n")
            ]
        );
    }

    #[test]
    fn test_multiple_prerequisites() {
        assert_eq!(
            lex(r#"rule: prerequisite1 prerequisite2
	recipe

"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            vec![
                (IDENTIFIER, "rule"),
                (COLON, ":"),
                (WHITESPACE, " "),
                (IDENTIFIER, "prerequisite1"),
                (WHITESPACE, " "),
                (IDENTIFIER, "prerequisite2"),
                (NEWLINE, "\n"),
                (INDENT, "\t"),
                (TEXT, "recipe"),
                (NEWLINE, "\n"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_variable_question() {
        assert_eq!(
            lex("VARIABLE ?= value\n")
                .iter()
                .map(|(kind, text)| (*kind, text.as_str()))
                .collect::<Vec<_>>(),
            vec![
                (IDENTIFIER, "VARIABLE"),
                (WHITESPACE, " "),
                (QUESTION, "?"),
                (EQUALS, "="),
                (WHITESPACE, " "),
                (IDENTIFIER, "value"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_conditional() {
        assert_eq!(
            lex(r#"ifneq (a, b)
endif
"#)
            .iter()
            .map(|(kind, text)| (*kind, text.as_str()))
            .collect::<Vec<_>>(),
            vec![
                (IDENTIFIER, "ifneq"),
                (WHITESPACE, " "),
                (LPAREN, "("),
                (IDENTIFIER, "a"),
                (COMMA, ","),
                (WHITESPACE, " "),
                (IDENTIFIER, "b"),
                (RPAREN, ")"),
                (NEWLINE, "\n"),
                (IDENTIFIER, "endif"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_variable_paren() {
        assert_eq!(
            lex("VARIABLE = $(value)\n")
                .iter()
                .map(|(kind, text)| (*kind, text.as_str()))
                .collect::<Vec<_>>(),
            vec![
                (IDENTIFIER, "VARIABLE"),
                (WHITESPACE, " "),
                (EQUALS, "="),
                (WHITESPACE, " "),
                (DOLLAR, "$"),
                (LPAREN, "("),
                (IDENTIFIER, "value"),
                (RPAREN, ")"),
                (NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_variable_paren2() {
        assert_eq!(
            lex("VARIABLE = $(value)$(value2)\n")
                .iter()
                .map(|(kind, text)| (*kind, text.as_str()))
                .collect::<Vec<_>>(),
            vec![
                (IDENTIFIER, "VARIABLE"),
                (WHITESPACE, " "),
                (EQUALS, "="),
                (WHITESPACE, " "),
                (DOLLAR, "$"),
                (LPAREN, "("),
                (IDENTIFIER, "value"),
                (RPAREN, ")"),
                (DOLLAR, "$"),
                (LPAREN, "("),
                (IDENTIFIER, "value2"),
                (RPAREN, ")"),
                (NEWLINE, "\n"),
            ]
        );
    }
}

mod parse {
    use posixutils_make::parser::preprocessor::preprocess;
    use posixutils_make::parser::{parse::parse, Makefile};
    use rowan::ast::AstNode;

    #[test]
    fn test_parse_simple() {
        const SIMPLE: &str = r#"VARIABLE = command2

rule: dependency
	command
	${VARIABLE}

"#;
        let Ok(processed) = preprocess(SIMPLE) else {
            panic!("Must be preprocessed without an error")
        };
        let parsed = parse(&processed);
        println!("{:#?}", parsed.clone().unwrap().syntax());
        assert_eq!(parsed.clone().err(), None);
        let node = parsed.clone().unwrap().syntax();
        assert_eq!(
            format!("{:#?}", node),
            r#"ROOT@0..38
  NEWLINE@0..1 "\n"
  RULE@1..38
    IDENTIFIER@1..5 "rule"
    COLON@5..6 ":"
    WHITESPACE@6..7 " "
    EXPR@7..17
      IDENTIFIER@7..17 "dependency"
    NEWLINE@17..18 "\n"
    RECIPE@18..27
      INDENT@18..19 "\t"
      TEXT@19..26 "command"
      NEWLINE@26..27 "\n"
    RECIPE@27..37
      INDENT@27..28 "\t"
      TEXT@28..36 "command2"
      NEWLINE@36..37 "\n"
    NEWLINE@37..38 "\n"
"#
        );

        let root = parsed.unwrap().root().clone_for_update();

        let mut rules = root.rules().collect::<Vec<_>>();
        assert_eq!(rules.len(), 1);
        let rule = rules.pop().unwrap();
        assert_eq!(rule.targets().collect::<Vec<_>>(), vec!["rule"]);
        assert_eq!(rule.prerequisites().collect::<Vec<_>>(), vec!["dependency"]);
        assert_eq!(
            rule.recipes().collect::<Vec<_>>(),
            vec!["command", "command2"]
        );
    }

    #[test]
    fn test_parse_export_assign() {
        const EXPORT: &str = r#"export VARIABLE := value
"#;
        let Ok(processed) = preprocess(EXPORT).map_err(|e| println!("{e:?}")) else {
            panic!("Must be preprocessed without an error")
        };
        let parsed = parse(&processed);
        assert!(parsed.clone().err().is_some());
    }

    // TODO: create `include` test with real files
    //
    //     #[test]
    //     fn test_parse_include() {
    //         const INCLUDE: &str = r#"include FILENAME
    // "#;
    //         let Ok(processed) = preprocess(INCLUDE) else { panic!("Could not preprocess") };
    //         let parsed = parse(&processed);
    //         assert_eq!(parsed.errors, Vec::<String>::new());
    //         let node = parsed.syntax();
    //
    //         assert_eq!(
    //             format!("{:#?}", node),
    //             r#"ROOT@0..17
    //   IDENTIFIER@0..7 "include"
    //   WHITESPACE@7..8 " "
    //   IDENTIFIER@8..16 "FILENAME"
    //   NEWLINE@16..17 "\n"
    // "#
    //         );
    //
    //         let root = parsed.root().clone_for_update();
    //
    //         let variables = root.syntax();
    //         dbg!(&variables);
    //         // assert_eq!(variables.len(), 1);
    //         // let variable = variables.pop().unwrap();
    //         // assert_eq!(variable.name(), Some("VARIABLE".to_string()));
    //         // assert_eq!(variable.raw_value(), Some("value".to_string()));
    //     }

    #[test]
    fn test_parse_multiple_prerequisites() {
        const MULTIPLE_PREREQUISITES: &str = r#"rule: dependency1 dependency2
	command

"#;
        let parsed = parse(MULTIPLE_PREREQUISITES);
        assert_eq!(parsed.clone().err(), None);
        let node = parsed.clone().unwrap().syntax();
        assert_eq!(
            format!("{:#?}", node),
            r#"ROOT@0..40
  RULE@0..40
    IDENTIFIER@0..4 "rule"
    COLON@4..5 ":"
    WHITESPACE@5..6 " "
    EXPR@6..29
      IDENTIFIER@6..17 "dependency1"
      WHITESPACE@17..18 " "
      IDENTIFIER@18..29 "dependency2"
    NEWLINE@29..30 "\n"
    RECIPE@30..39
      INDENT@30..31 "\t"
      TEXT@31..38 "command"
      NEWLINE@38..39 "\n"
    NEWLINE@39..40 "\n"
"#
        );
        let root = parsed.unwrap().root().clone_for_update();

        let rule = root.rules().next().unwrap();
        assert_eq!(rule.targets().collect::<Vec<_>>(), vec!["rule"]);
        assert_eq!(
            rule.prerequisites().collect::<Vec<_>>(),
            vec!["dependency1", "dependency2"]
        );
        assert_eq!(rule.recipes().collect::<Vec<_>>(), vec!["command"]);
    }

    #[test]
    fn test_add_rule() {
        let mut makefile = Makefile::new();
        let rule = makefile.add_rule("rule");
        assert_eq!(rule.targets().collect::<Vec<_>>(), vec!["rule"]);
        assert_eq!(
            rule.prerequisites().collect::<Vec<_>>(),
            Vec::<String>::new()
        );

        assert_eq!(makefile.to_string(), "rule:\n");
    }

    #[test]
    fn test_push_command() {
        let mut makefile = Makefile::new();
        let rule = makefile.add_rule("rule");
        rule.push_command("command");
        assert_eq!(rule.recipes().collect::<Vec<_>>(), vec!["command"]);

        assert_eq!(makefile.to_string(), "rule:\n\tcommand\n");

        rule.push_command("command2");
        assert_eq!(
            rule.recipes().collect::<Vec<_>>(),
            vec!["command", "command2"]
        );

        assert_eq!(makefile.to_string(), "rule:\n\tcommand\n\tcommand2\n");
    }

    #[test]
    fn test_replace_command() {
        let mut makefile = Makefile::new();
        let rule = makefile.add_rule("rule");
        rule.push_command("command");
        rule.push_command("command2");
        assert_eq!(
            rule.recipes().collect::<Vec<_>>(),
            vec!["command", "command2"]
        );

        rule.replace_command(0, "new command");
        assert_eq!(
            rule.recipes().collect::<Vec<_>>(),
            vec!["new command", "command2"]
        );

        assert_eq!(makefile.to_string(), "rule:\n\tnew command\n\tcommand2\n");
    }
}
