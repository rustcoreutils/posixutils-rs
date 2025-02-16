use crate::interpreter::wordexp::pattern::Pattern;
use crate::interpreter::wordexp::{
    expand_word_to_string, simple_word_expansion_into, ExpandedWord, ExpandedWordPart,
};
use crate::interpreter::{Interpreter, Variable};
use crate::program::{Parameter, ParameterExpansion, SpecialParameter};

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
    str: Option<&String>,
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
    interpreter: &mut Interpreter,
) -> ParameterExpansionResult {
    match parameter {
        Parameter::Number(n) => add_option_to_expanded_word(
            expanded_word,
            interpreter.positional_parameters.get(*n as usize - 1),
            inside_double_quotes,
        ),
        Parameter::Variable(var_name) => add_option_to_expanded_word(
            expanded_word,
            interpreter
                .environment
                .get(var_name.as_ref())
                .map(|v| &v.value),
            inside_double_quotes,
        ),
        Parameter::Special(special_parameter) => {
            match special_parameter {
                SpecialParameter::At => {
                    if !field_splitting_will_be_performed {
                        expanded_word.append(
                            interpreter.positional_parameters.join(" "),
                            inside_double_quotes,
                            true,
                        );
                    } else {
                        add_split_parameters_to_expanded_word(
                            expanded_word,
                            &interpreter.positional_parameters,
                            inside_double_quotes,
                        );
                    }
                }
                SpecialParameter::Asterisk => {
                    if field_splitting_will_be_performed && !inside_double_quotes {
                        add_split_parameters_to_expanded_word(
                            expanded_word,
                            &interpreter.positional_parameters,
                            false,
                        );
                    } else {
                        let separator = interpreter
                            .environment
                            .get("IFS")
                            .map(|var| {
                                if var.value.is_empty() {
                                    ""
                                } else {
                                    &var.value[..1]
                                }
                            })
                            .unwrap_or(" ");
                        expanded_word.append(
                            interpreter.positional_parameters.join(separator),
                            inside_double_quotes,
                            true,
                        );
                    }
                }
                SpecialParameter::Hash => {
                    expanded_word.append(
                        interpreter.positional_parameters.len().to_string(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::QuestionMark => {
                    expanded_word.append(
                        interpreter.most_recent_pipeline_exit_status.to_string(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::Minus => {
                    expanded_word.append(
                        interpreter.set_options.to_string_short(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::Dollar => {
                    expanded_word.append(
                        interpreter.shell_pid.to_string(),
                        inside_double_quotes,
                        true,
                    );
                }
                SpecialParameter::Bang => expanded_word.append(
                    interpreter
                        .most_recent_background_command_pid
                        .map(|pid| pid.to_string())
                        .unwrap_or_default(),
                    inside_double_quotes,
                    true,
                ),
                SpecialParameter::Zero => {
                    expanded_word.append(
                        interpreter.program_name.clone(),
                        inside_double_quotes,
                        true,
                    );
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
    interpreter: &mut Interpreter,
) {
    match parameter_expansion {
        ParameterExpansion::Simple(parameter) => {
            expand_simple_parameter_into(
                expanded_word,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                interpreter,
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
                interpreter,
            );
            if parameter_type.is_unset() || (*default_on_null && parameter_type.is_null()) {
                if let Some(default) = default {
                    simple_word_expansion_into(expanded_word, default, false, interpreter);
                }
            }
            expanded_word.extend(expanded_parameter);
        }
        ParameterExpansion::UnsetAssignDefault {
            parameter,
            word,
            assign_on_null,
        } => {
            match parameter {
                Parameter::Number(_) | Parameter::Special(_) => {
                    todo!("error: cannot assign to positional argument or special parameter")
                }
                Parameter::Variable(variable_name) => {
                    let value = word
                        .as_ref()
                        .map(|w| expand_word_to_string(w, false, interpreter))
                        .unwrap_or_default();
                    match interpreter.environment.get_mut(variable_name.as_ref()) {
                        Some(variable) if *assign_on_null && variable.value.is_empty() => {
                            variable.value = value.clone();
                        }
                        None => {
                            interpreter
                                .environment
                                .insert(variable_name.to_string(), Variable::new(value.clone()));
                        }
                        _ => {
                            // variable is set and not null
                        }
                    }
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
                interpreter,
            );
            if parameter_type.is_unset() || (*error_on_null && parameter_type.is_null()) {
                if let Some(word) = word {
                    eprintln!("{}", expand_word_to_string(word, false, interpreter));
                } else if *error_on_null {
                    eprintln!("parameter is unset or null");
                } else {
                    eprintln!("parameter is unset");
                }
                std::process::exit(1);
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
                interpreter,
            );
            if !parameter_type.is_unset()
                && (!parameter_type.is_null() || *substitute_null_with_word)
            {
                if let Some(word) = word {
                    simple_word_expansion_into(expanded_word, word, false, interpreter)
                }
            }
        }
        ParameterExpansion::StrLen(parameter) => {
            if matches!(
                parameter,
                Parameter::Special(SpecialParameter::Asterisk)
                    | Parameter::Special(SpecialParameter::At)
            ) {
                todo!("error: length of '*' or '@' is unspecified")
            }
            let mut expanded_parameter = ExpandedWord::default();
            let parameter_type = expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                false,
                false,
                interpreter,
            );
            if parameter_type.is_unset() && interpreter.set_options.nounset {
                todo!("error: unset parameter")
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
            expand_simple_parameter_into(
                &mut expanded_parameter,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                interpreter,
            );
            let param_str = expanded_parameter.to_string();
            if let Some(word) = pattern {
                let mut expanded_pattern = ExpandedWord::default();
                simple_word_expansion_into(&mut expanded_pattern, word, false, interpreter);
                // TODO: fix unwrap
                let pattern = Pattern::new(&expanded_pattern).unwrap();
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
            } else {
                expanded_word.append(param_str, inside_double_quotes, true);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::test_utils::unquoted_literal;

    fn interpreter_with_env(env: &[(&str, &str)]) -> Interpreter {
        let mut interpreter = Interpreter::default();
        for (k, v) in env {
            interpreter
                .environment
                .insert(k.to_string(), Variable::new(v.to_string()));
        }
        interpreter
    }

    fn interpreter_with_positional_arguments(args: Vec<&str>) -> Interpreter {
        let mut interpreter = Interpreter::default();
        interpreter.positional_parameters = args.iter().map(|s| s.to_string()).collect();
        interpreter
    }

    fn expand_parameter_to_string(
        parameter_expansion: ParameterExpansion,
        interpreter: &mut Interpreter,
    ) -> String {
        let mut expanded_word = ExpandedWord::default();
        expand_parameter_into(
            &mut expanded_word,
            &parameter_expansion,
            false,
            false,
            interpreter,
        );
        expanded_word.to_string()
    }

    fn expand_parameter(
        parameter_expansion: ParameterExpansion,
        inside_double_quotes: bool,
        field_splitting_will_be_performed: bool,
        interpreter: &mut Interpreter,
    ) -> ExpandedWord {
        let mut expanded_word = ExpandedWord::default();
        expand_parameter_into(
            &mut expanded_word,
            &parameter_expansion,
            inside_double_quotes,
            field_splitting_will_be_performed,
            interpreter,
        );
        expanded_word
    }

    #[test]
    fn expand_simple_named_parameter() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test_user")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("HOME".into())),
                &mut interpreter
            ),
            "/home/test_user"
        );
    }

    #[test]
    fn expand_dollar() {
        let mut interpreter = Interpreter::default();
        interpreter.shell_pid = 123;
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Dollar)),
                &mut interpreter
            ),
            "123"
        );
    }

    #[test]
    fn expand_bang() {
        let mut interpreter = Interpreter::default();
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang)),
                &mut interpreter
            ),
            "".to_string()
        );
        interpreter.most_recent_background_command_pid = Some(123);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang)),
                &mut interpreter
            ),
            "123".to_string()
        );
    }

    #[test]
    fn unset_use_default_parameter_expansion() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test_user"), ("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("HOME".into()),
                    word: None,
                    default_on_null: false,
                },
                &mut interpreter
            ),
            "/home/test_user"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: Some(unquoted_literal("default")),
                    default_on_null: false,
                },
                &mut interpreter
            ),
            "default"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    default_on_null: false,
                },
                &mut interpreter
            ),
            ""
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    default_on_null: true,
                },
                &mut interpreter
            ),
            "default"
        );
    }

    #[test]
    fn unset_assign_default_parameter_expansion() {
        let mut interpreter = interpreter_with_env(&[("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: Some(unquoted_literal("value")),
                    assign_on_null: false,
                },
                &mut interpreter
            ),
            "value"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("unset_var".into())),
                &mut interpreter
            ),
            "value"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: None,
                    assign_on_null: false,
                },
                &mut interpreter
            ),
            ""
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("unset_var".into())),
                &mut interpreter
            ),
            "value".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    assign_on_null: false,
                },
                &mut interpreter
            ),
            "default".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("NULL".into())),
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    assign_on_null: true,
                },
                &mut interpreter
            ),
            "default".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Variable("NULL".into())),
                &mut interpreter
            ),
            "default".to_string()
        );
    }

    #[test]
    fn set_use_alternative_parameter_expansion() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test"), ("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("HOME".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: false,
                },
                &mut interpreter
            ),
            "word".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: true,
                },
                &mut interpreter
            ),
            "word".to_string()
        );
    }

    #[test]
    fn string_length_parameter_expansion() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test_user")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::StrLen(Parameter::Variable("HOME".into())),
                &mut interpreter
            ),
            "15".to_string()
        );
    }

    #[test]
    fn remove_smallest_suffix() {
        let mut interpreter = interpreter_with_env(&[
            ("HOME", "/home/test_user"),
            ("TEST", "aabbcc"),
            ("NULL", ""),
        ]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Some(unquoted_literal("test_user")),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "/home/".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: Some(unquoted_literal("a*c")),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "a".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: None,
                    remove_largest: false,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_largest_suffix() {
        let mut interpreter = interpreter_with_env(&[
            ("HOME", "/home/test_user"),
            ("TEST", "aabbcc"),
            ("NULL", ""),
        ]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Some(unquoted_literal("test_user")),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "/home/".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: Some(unquoted_literal("a*c")),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: None,
                    remove_largest: true,
                    remove_prefix: false,
                },
                &mut interpreter
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_smallest_prefix() {
        let mut interpreter = interpreter_with_env(&[
            ("HOME", "/home/test_user"),
            ("TEST", "aabbcc"),
            ("NULL", ""),
        ]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Some(unquoted_literal("/home/")),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "test_user".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: Some(unquoted_literal("a*c")),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "c".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: None,
                    remove_largest: false,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn remove_largest_prefix() {
        let mut interpreter =
            interpreter_with_env(&[("HOME", "/home/test_user"), ("TEST", "aabbc"), ("NULL", "")]);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: Some(unquoted_literal("/home/")),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "test_user"
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("TEST".into()),
                    pattern: Some(unquoted_literal("a*c")),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("NULL".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("UNDEFINED".into()),
                    pattern: Some(unquoted_literal("anything")),
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::RemovePattern {
                    parameter: Parameter::Variable("HOME".into()),
                    pattern: None,
                    remove_largest: true,
                    remove_prefix: true,
                },
                &mut interpreter
            ),
            "/home/test_user".to_string()
        );
    }

    #[test]
    fn expand_at() {
        let mut interpreter = interpreter_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At)),
                false,
                false,
                &mut interpreter
            ),
            ExpandedWord::generated_unquoted_literal("arg1 arg2 arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At)),
                false,
                true,
                &mut interpreter
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
                &mut interpreter
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
        let mut interpreter = interpreter_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut interpreter
            ),
            ExpandedWord::generated_unquoted_literal("arg1 arg2 arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut interpreter
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
                &mut interpreter
            ),
            ExpandedWord::quoted_literal("arg1 arg2 arg3")
        );
    }

    #[test]
    fn expand_asterisk_with_null_ifs() {
        let mut interpreter = interpreter_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        interpreter
            .environment
            .insert("IFS".to_string(), Variable::new("".to_string()));
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut interpreter
            ),
            ExpandedWord::generated_unquoted_literal("arg1arg2arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut interpreter
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
                &mut interpreter
            ),
            ExpandedWord::quoted_literal("arg1arg2arg3")
        );
    }

    #[test]
    fn expand_asterisk_with_unset_ifs() {
        let mut interpreter = interpreter_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        interpreter.environment.remove("IFS");
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut interpreter
            ),
            ExpandedWord::generated_unquoted_literal("arg1 arg2 arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut interpreter
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
                &mut interpreter
            ),
            ExpandedWord::quoted_literal("arg1 arg2 arg3")
        );
    }

    #[test]
    fn expand_asterisk_with_custom_ifs() {
        let mut interpreter = interpreter_with_positional_arguments(vec!["arg1", "arg2", "arg3"]);
        interpreter
            .environment
            .insert("IFS".to_string(), Variable::new(",:".to_string()));
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                false,
                &mut interpreter
            ),
            ExpandedWord::generated_unquoted_literal("arg1,arg2,arg3")
        );
        assert_eq!(
            expand_parameter(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk)),
                false,
                true,
                &mut interpreter
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
                &mut interpreter
            ),
            ExpandedWord::quoted_literal("arg1,arg2,arg3")
        );
    }
}
