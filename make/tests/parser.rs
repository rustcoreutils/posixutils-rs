// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod preprocess {
    use posixutils_make::parser::preprocessor::{generate_macro_table, preprocess};
    use posixutils_make::rule::target::Target;
    use std::path::PathBuf;

    #[test]
    fn test_macros_simple() {
        const MACROS: &str = r#"VAR = var
V = ok

all:
    $(VAR) $V ${VAR} ${V} $(V)"#;

        const EXPECTED: &str = r#"
all:
    var ok var ok ok
"#;
        let table = generate_macro_table(
            MACROS,
            &Target::Simple { name: "test" },
            &(PathBuf::new(), PathBuf::new()),
            [].iter(),
        )
        .unwrap();
        let Ok(result) = preprocess(
            MACROS,
            &table,
            &Target::Simple { name: "test" },
            &(PathBuf::new(), PathBuf::new()),
            [].iter(),
        ) else {
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
                (EQUALS, "="),
                (IDENTIFIER, "value"),
                (NEWLINE, "\n"),
                (NEWLINE, "\n"),
                (IDENTIFIER, "rule"),
                (COLON, ":"),
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
                (IDENTIFIER, "VARIABLE"),
                (COLON, ":"),
                (EQUALS, "="),
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
                (IDENTIFIER, "prerequisite1"),
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
                (QUESTION, "?"),
                (EQUALS, "="),
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
                (LPAREN, "("),
                (IDENTIFIER, "a"),
                (COMMA, ","),
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
                (EQUALS, "="),
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
                (EQUALS, "="),
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
    use posixutils_make::parser::preprocessor::{generate_macro_table, preprocess};
    use posixutils_make::parser::{parse::parse, Makefile};
    use posixutils_make::rule::target::Target;
    use rowan::ast::AstNode;
    use std::path::PathBuf;

    #[test]
    fn test_parse_simple() {
        const SIMPLE: &str = r#"VARIABLE = command2

        rule: dependency
        	command
        	${VARIABLE}

        "#;
        let table = generate_macro_table(
            SIMPLE,
            &Target::Simple { name: "test" },
            &(PathBuf::new(), PathBuf::new()),
            [].iter(),
        )
        .unwrap();
        let Ok(processed) = preprocess(
            SIMPLE,
            &table,
            &Target::Simple { name: "test" },
            &(PathBuf::new(), PathBuf::new()),
            [].iter(),
        ) else {
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
  RULE@1..37
    IDENTIFIER@1..5 "rule"
    COLON@5..6 ":"
    EXPR@6..16
      IDENTIFIER@6..16 "dependency"
    NEWLINE@16..17 "\n"
    RECIPE@17..26
      INDENT@17..18 "\t"
      TEXT@18..25 "command"
      NEWLINE@25..26 "\n"
    RECIPE@26..36
      INDENT@26..27 "\t"
      TEXT@27..35 "command2"
      NEWLINE@35..36 "\n"
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
        let table = generate_macro_table(
            EXPORT,
            &Target::Simple { name: "test" },
            &(PathBuf::new(), PathBuf::new()),
            [].iter(),
        )
        .unwrap();
        let Ok(processed) = preprocess(
            EXPORT,
            &table,
            &Target::Simple { name: "test" },
            &(PathBuf::new(), PathBuf::new()),
            [].iter(),
        )
        .map_err(|e| println!("{e:?}")) else {
            panic!("Must be preprocessed without an error")
        };
        let parsed = parse(&processed);
        assert!(parsed.clone().err().is_none());
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
            r#"ROOT@0..38
  RULE@0..38
    IDENTIFIER@0..4 "rule"
    COLON@4..5 ":"
    EXPR@5..27
      IDENTIFIER@5..16 "dependency1"
      IDENTIFIER@16..27 "dependency2"
    NEWLINE@27..28 "\n"
    RECIPE@28..37
      INDENT@28..29 "\t"
      TEXT@29..36 "command"
      NEWLINE@36..37 "\n"
    NEWLINE@37..38 "\n"
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
