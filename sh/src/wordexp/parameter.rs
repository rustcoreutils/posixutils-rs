use crate::parse::word::{Parameter, ParameterExpansion, SpecialParameter};
use crate::shell::{CommandExecutionError, Shell};
use crate::wordexp::{
    expand_word_to_string, simple_word_expansion_into, word_to_pattern, ExpandedWord,
    ExpansionResult,
};

#[derive(PartialEq, Eq)]
enum ParameterExpansionResult {
    Unset,
    Set,
    Null,
}

impl ParameterExpansionResult {
    fn is_null(&self) -> bool {
        self == &ParameterExpansionResult::Null
    }

    fn is_unset(&self) -> bool {
        self == &ParameterExpansionResult::Unset
    }
}

fn add_option_to_expanded_word(
    word: &mut ExpandedWord,
    str: Option<&str>,
    inside_double_quotes: bool,
) -> ParameterExpansionResult {
    if let Some(s) = str {
        word.append(s, inside_double_quotes, true);
        if s.is_empty() {
            ParameterExpansionResult::Null
        } else {
            ParameterExpansionResult::Set
        }
    } else {
        ParameterExpansionResult::Unset
    }
}

fn add_split_parameters_to_expanded_word(
    word: &mut ExpandedWord,
    parameters: &[String],
    quoted: bool,
) {
    if parameters.is_empty() {
        return;
    }
    let mut i = 0;
    while i < parameters.len() - 1 {
        word.append(&parameters[i], quoted, true);
        word.end_field();
        i += 1;
    }
    word.append(&parameters[i], quoted, true);
}

fn expand_simple_parameter_into(
    expanded_word: &mut ExpandedWord,
    parameter: &Parameter,
    inside_double_quotes: bool,
    field_splitting_will_be_performed: bool,
    shell: &mut Shell,
) -> ParameterExpansionResult {
    match parameter {
        Parameter::Number(n) => add_option_to_expanded_word(
            expanded_word,
            shell
                .positional_parameters
                .get(*n as usize - 1)
                .map(|s| s.as_str()),
            inside_double_quotes,
        ),
        Parameter::Variable(var_name) => add_option_to_expanded_word(
            expanded_word,
            shell.environment.get_str_value(var_name.as_ref()),
            inside_double_quotes,
        ),
        Parameter::Special(special_parameter) => {
            match special_parameter {
                SpecialParameter::At => {
                    if !field_splitting_will_be_performed {
                        expanded_word.append(
                            shell.positional_parameters.join(" "),
                            inside_double_quotes,
                            true,
                        );
                    } else {
                        add_split_parameters_to_expanded_word(
                            expanded_word,
                            &shell.positional_parameters,
                            inside_double_quotes,
                        );
                    }
                }
                SpecialParameter::Asterisk => {
                    if field_splitting_will_be_performed && !inside_double_quotes {
                        add_split_parameters_to_expanded_word(
                            expanded_word,
                            &shell.positional_parameters,
                            false,
                        );
                    } else {
                        let separator = shell
                            .environment
                            .get_str_value("IFS")
                            .map(|v| if v.is_empty() { "" } else { &v[..1] })
                            .unwrap_or(" ");
                        expanded_word.append(
                            shell.positional_parameters.join(separator),
                            inside_double_quotes,
                            true,
                        );
                    }
                }
                SpecialParameter::Hash => {
                    expanded_word.append(
                        shell.positional_parameters.len().to_string(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::QuestionMark => {
                    expanded_word.append(
                        shell.most_recent_pipeline_exit_status.to_string(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::Minus => {
                    expanded_word.append(
                        shell.set_options.to_string_short(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::Dollar => {
                    expanded_word.append(shell.shell_pid.to_string(), inside_double_quotes, true);
                }
                SpecialParameter::Bang => expanded_word.append(
                    shell
                        .most_recent_background_command_pid
                        .map(|pid| pid.to_string())
                        .unwrap_or_default(),
                    inside_double_quotes,
                    true,
                ),
                SpecialParameter::Zero => {
                    expanded_word.append(shell.program_name.clone(), inside_double_quotes, true);
                }
            }
            // special parameters are always set
            ParameterExpansionResult::Set
        }
    }
}

pub fn expand_parameter_into(
    expanded_word: &mut ExpandedWord,
    parameter_expansion: &ParameterExpansion,
    inside_double_quotes: bool,
    field_splitting_will_be_performed: bool,
    shell: &mut Shell,
) -> ExpansionResult<()> {
    match parameter_expansion {
        ParameterExpansion::Simple(parameter) => {
            expand_simple_parameter_into(
                expanded_word,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                shell,
            );
        }
        ParameterExpansion::UnsetUseDefault {
            parameter,
            word: default,
            default_on_null,
        } => {
            let mut expanded_parameter = ExpandedWord::default();
            let parameter_type = expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                shell,
            );
            if parameter_type.is_unset() || (*default_on_null && parameter_type.is_null()) {
                simple_word_expansion_into(expanded_word, default, false, shell)?;
            }
            expanded_word.extend(expanded_parameter);
        }
        ParameterExpansion::UnsetAssignDefault {
            variable: variable_name,
            word,
            assign_on_null,
        } => {
            let value = expand_word_to_string(word, false, shell)?;
            match shell.environment.get_var_mut(variable_name.as_ref()) {
                Some(variable) => {
                    if !variable.is_set() || (variable.is_null() && *assign_on_null) {
                        if variable.readonly {
                            return Err(CommandExecutionError::ExpansionError(format!(
                                "sh: cannot set readonly variable {variable_name}"
                            )));
                        }
                        variable.value = Some(value.clone());
                        expanded_word.append(value, inside_double_quotes, true);
                    } else {
                        expanded_word.append(
                            variable.value.clone().unwrap(),
                            inside_double_quotes,
                            true,
                        );
                    }
                }
                None => {
                    // cannot fail since var is not in the environment
                    let _ = shell
                        .environment
                        .set(variable_name.to_string(), value.clone(), false);
                    expanded_word.append(value, inside_double_quotes, true);
                }
            }
        }
        ParameterExpansion::UnsetError {
            parameter,
            word,
            error_on_null,
        } => {
            let mut expanded_parameter = ExpandedWord::default();
            let parameter_type = expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                shell,
            );
            if parameter_type.is_unset() || (*error_on_null && parameter_type.is_null()) {
                return if word.parts.is_empty() {
                    let message = expand_word_to_string(word, false, shell)?;
                    Err(CommandExecutionError::ExpansionError(message))
                } else if *error_on_null {
                    Err(CommandExecutionError::ExpansionError(
                        "parameter is unset or null".to_string(),
                    ))
                } else {
                    Err(CommandExecutionError::ExpansionError(
                        "parameter is unset".to_string(),
                    ))
                };
            }
            expanded_word.extend(expanded_parameter);
        }
        ParameterExpansion::SetUseAlternative {
            parameter,
            word,
            substitute_null_with_word,
        } => {
            let mut expanded_parameter = ExpandedWord::default();
            let parameter_type = expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                shell,
            );
            if !parameter_type.is_unset()
                && (!parameter_type.is_null() || *substitute_null_with_word)
            {
                simple_word_expansion_into(expanded_word, word, false, shell)?
            }
        }
        ParameterExpansion::StrLen(parameter) => {
            let mut expanded_parameter = ExpandedWord::default();
            let parameter_type = expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                false,
                false,
                shell,
            );
            if parameter_type.is_unset() && shell.set_options.nounset {
                return Err(CommandExecutionError::ExpansionError(
                    "sh: parameter is unset".to_string(),
                ));
            }
            expanded_word.append(
                expanded_parameter.to_string().len().to_string(),
                inside_double_quotes,
                true,
            );
        }
        ParameterExpansion::RemovePattern {
            parameter,
            pattern,
            remove_prefix,
            remove_largest,
        } => {
            let mut expanded_parameter = ExpandedWord::default();
            let parameter_type = expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                shell,
            );
            if parameter_type.is_unset() && shell.set_options.nounset {
                return Err(CommandExecutionError::ExpansionError(
                    "sh: parameter is unset".to_string(),
                ));
            }
            let param_str = expanded_parameter.to_string();

            let pattern = word_to_pattern(pattern, shell)?;
            let result = if *remove_prefix {
                if *remove_largest {
                    pattern.remove_largest_prefix(param_str)
                } else {
                    pattern.remove_shortest_prefix(param_str)
                }
            } else {
                if *remove_largest {
                    pattern.remove_largest_suffix(param_str)
                } else {
                    pattern.remove_shortest_suffix(param_str)
                }
            };
            expanded_word.append(result, inside_double_quotes, true);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::word::test_utils::unquoted_literal;
    use crate::parse::word::Word;
    use crate::wordexp::expanded_word::ExpandedWordPart;

    fn shell_with_env(env: &[(&str, &str)]) -> Shell {
        let mut shell = Shell::default();
        for (k, v) in env {
            shell
                .environment
                .set(k.to_string(), v.to_string(), false)
                .expect("failed to set var");
        }
        shell
    }

    fn shell_with_positional_arguments(args: Vec<&str>) -> Shell {
        let mut shell = Shell::default();
        shell.positional_parameters = args.iter().map(|s| s.to_string()).collect();
        shell
    }

    fn expand_parameter_to_string(
        parameter_expansion: ParameterExpansion,
        shell: &mut Shell,
    ) -> String {
        let mut expanded_word = ExpandedWord::default();
        expand_parameter_into(
            &mut expanded_word,
            &parameter_expansion,
            false,
            false,
            shell,
        );
        expanded_word.to_string()
    }

    fn expand_parameter(
        parameter_expansion: ParameterExpansion,
        inside_double_quotes: bool,
        field_splitting_will_be_performed: bool,
        shell: &mut Shell,
    ) -> ExpandedWord {
        let mut expanded_word = ExpandedWord::default();
        expand_parameter_into(
            &mut expanded_word,
            &parameter_expansion,
            inside_double_quotes,
            field_splitting_will_be_performed,
            shell,
        );
        expanded_word
    }

    #[test]
    fn expand_simple_named_parameter() {
        let mut shell = shell_with_env(&[("HOME", "/home/test_user")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("HOME".into())),
                &mut shell
            ),
            "/home/test_user"
        );
    }

    #[test]
    fn expand_dollar() {
        let mut shell = Shell::default();
        shell.shell_pid = 123;
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Dollar)),
                &mut shell
            ),
            "123"
        );
    }

    #[test]
    fn expand_bang() {
        let mut shell = Shell::default();
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang)),
                &mut shell
            ),
            "".to_string()
        );
        shell.most_recent_background_command_pid = Some(123);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang)),
                &mut shell
            ),
            "123".to_string()
        );
    }

    #[test]
    fn unset_use_default_parameter_expansion() {
        let mut shell = shell_with_env(&[("HOME", "/home/test_user"), ("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("HOME".into()),
                    word: Word::default(),
                    default_on_null: false,
                },
                &mut shell
            ),
            "/home/test_user"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: unquoted_literal("default"),
                    default_on_null: false,
                },
                &mut shell
            ),
            "default"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: unquoted_literal("default"),
                    default_on_null: false,
                },
                &mut shell
            ),
            ""
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: unquoted_literal("default"),
                    default_on_null: true,
                },
                &mut shell
            ),
            "default"
        );
    }

    #[test]
    fn unset_assign_default_parameter_expansion() {
        let mut shell = shell_with_env(&[("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    variable: "unset_var".into(),
                    word: unquoted_literal("value"),
                    assign_on_null: false,
                },
                &mut shell
            ),
            "value"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("unset_var".into())),
                &mut shell
            ),
            "value"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    variable: "unset_var".into(),
                    word: Word::default(),
                    assign_on_null: false,
                },
                &mut shell
            ),
            "value"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("unset_var".into())),
                &mut shell
            ),
            "value".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    variable: "NULL".into(),
                    word: unquoted_literal("default"),
                    assign_on_null: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("NULL".into())),
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    variable: "NULL".into(),
                    word: unquoted_literal("default"),
                    assign_on_null: true,
                },
                &mut shell
            ),
            "default".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("NULL".into())),
                &mut shell
            ),
            "default".to_string()
        );
    }

    #[test]
    fn set_use_alternative_parameter_expansion() {
        let mut shell = shell_with_env(&[("HOME", "/home/test"), ("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("HOME".into()),
                    word: unquoted_literal("word"),
                    substitute_null_with_word: false,
                },
                &mut shell
            ),
            "word".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: unquoted_literal("word"),
                    substitute_null_with_word: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("NULL".into()),
                    word: unquoted_literal("word"),
                    substitute_null_with_word: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("NULL".into()),
                    word: unquoted_literal("word"),
                    substitute_null_with_word: true,
                },
                &mut shell
            ),
            "word".to_string()
        );
    }

    #[test]
    fn string_length_parameter_expansion() {
        let mut shell = shell_with_env(&[("HOME", "/home/test_user")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::StrLen(Parameter::Variable("HOME".into())),
                &mut shell
            ),
            "15".to_string()
        );
    }

    #[test]
    fn remove_smallest_suffix() {
        let mut shell = shell_with_env(&[
            ("HOME", "/home/test_user"),
            ("TEST", "aabbcc"),
            ("NULL", ""),
        ]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: unquoted_literal("test_user"),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "/home/".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: unquoted_literal("a*c"),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "a".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Word::default(),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_largest_suffix() {
        let mut shell = shell_with_env(&[
            ("HOME", "/home/test_user"),
            ("TEST", "aabbcc"),
            ("NULL", ""),
        ]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: unquoted_literal("test_user"),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "/home/".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: unquoted_literal("a*c"),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Word::default(),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut shell
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_smallest_prefix() {
        let mut shell = shell_with_env(&[
            ("HOME", "/home/test_user"),
            ("TEST", "aabbcc"),
            ("NULL", ""),
        ]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: unquoted_literal("/home/"),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "test_user".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: unquoted_literal("a*c"),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "c".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Word::default(),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_largest_prefix() {
        let mut shell =
            shell_with_env(&[("HOME", "/home/test_user"), ("TEST", "aabbc"), ("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: unquoted_literal("/home/"),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "test_user"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: unquoted_literal("a*c"),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: unquoted_literal("anything"),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Word::default(),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut shell
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn expand_at() {
        let mut shell = shell_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At)),
                false,
                false,
                &mut shell
            ),
            ExpandedWord::generated_unquoted_literal("arg1 arg2 arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At)),
                false,
                true,
                &mut shell
            ),
            ExpandedWord::from_parts(vec![
                ExpandedWordPart::GeneratedUnquotedLiteral("arg1".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg2".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg3".to_string())
            ])
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At)),
                true,
                true,
                &mut shell
            ),
            ExpandedWord::from_parts(vec![
                ExpandedWordPart::QuotedLiteral("arg1".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::QuotedLiteral("arg2".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::QuotedLiteral("arg3".to_string())
            ])
        );
    }

    #[test]
    fn expand_asterisk_with_default_ifs() {
        let mut shell = shell_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut shell
            ),
            ExpandedWord::generated_unquoted_literal("arg1 arg2 arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut shell
            ),
            ExpandedWord::from_parts(vec![
                ExpandedWordPart::GeneratedUnquotedLiteral("arg1".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg2".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg3".to_string())
            ])
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                true,
                true,
                &mut shell
            ),
            ExpandedWord::quoted_literal("arg1 arg2 arg3")
        );
    }

    #[test]
    fn expand_asterisk_with_null_ifs() {
        let mut shell = shell_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        shell
            .environment
            .set("IFS".to_string(), "".to_string(), false);
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut shell
            ),
            ExpandedWord::generated_unquoted_literal("arg1arg2arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut shell
            ),
            ExpandedWord::from_parts(vec![
                ExpandedWordPart::GeneratedUnquotedLiteral("arg1".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg2".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg3".to_string())
            ])
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                true,
                true,
                &mut shell
            ),
            ExpandedWord::quoted_literal("arg1arg2arg3")
        );
    }

    #[test]
    fn expand_asterisk_with_unset_ifs() {
        let mut shell = shell_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        shell.environment.unset("IFS").expect("cannot unset IFS");
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut shell
            ),
            ExpandedWord::generated_unquoted_literal("arg1 arg2 arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut shell
            ),
            ExpandedWord::from_parts(vec![
                ExpandedWordPart::GeneratedUnquotedLiteral("arg1".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg2".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg3".to_string())
            ])
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                true,
                true,
                &mut shell
            ),
            ExpandedWord::quoted_literal("arg1 arg2 arg3")
        );
    }

    #[test]
    fn expand_asterisk_with_custom_ifs() {
        let mut shell = shell_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        shell
            .environment
            .set("IFS".to_string(), ",:".to_string(), false);
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut shell
            ),
            ExpandedWord::generated_unquoted_literal("arg1,arg2,arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut shell
            ),
            ExpandedWord::from_parts(vec![
                ExpandedWordPart::GeneratedUnquotedLiteral("arg1".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg2".to_string()),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral("arg3".to_string())
            ])
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                true,
                true,
                &mut shell
            ),
            ExpandedWord::quoted_literal("arg1,arg2,arg3")
        );
    }
}
