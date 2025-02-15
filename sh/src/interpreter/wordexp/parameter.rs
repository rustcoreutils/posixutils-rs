use crate::interpreter::wordexp::pattern::Pattern;
use crate::interpreter::wordexp::{expand_word_simple, expand_word_to_string};
use crate::interpreter::{Interpreter, Variable};
use crate::program::{Parameter, ParameterExpansion, SpecialParameter};

fn expand_simple_parameter(parameter: &Parameter, interpreter: &mut Interpreter) -> Option<String> {
    match parameter {
        Parameter::Number(n) => interpreter
            .positional_parameters
            .get(*n as usize - 1)
            .cloned(),
        Parameter::Variable(var_name) => interpreter
            .environment
            .get(var_name.as_ref())
            .map(|v| v.value.clone()),
        Parameter::Special(special_parameter) => match special_parameter {
            SpecialParameter::At => {
                todo!()
            }
            SpecialParameter::Asterisk => {
                todo!()
            }
            SpecialParameter::Hash => Some(interpreter.positional_parameters.len().to_string()),
            SpecialParameter::QuestionMark => {
                Some(interpreter.most_recent_pipeline_status.to_string())
            }
            SpecialParameter::Minus => {
                todo!()
            }
            SpecialParameter::Dollar => Some(interpreter.shell_pid.to_string()),
            SpecialParameter::Bang => {
                Some(interpreter.most_recent_background_command_pid.to_string())
            }
            SpecialParameter::Zero => Some(interpreter.program_name.clone()),
        },
    }
}

pub fn expand_parameter(
    parameter_expansion: &ParameterExpansion,
    interpreter: &mut Interpreter,
) -> String {
    match parameter_expansion {
        ParameterExpansion::Simple(parameter) => {
            expand_simple_parameter(parameter, interpreter).unwrap_or_default()
        }
        ParameterExpansion::UnsetUseDefault {
            parameter,
            word: default,
            default_on_null,
        } => {
            // > If parameter is unset [or null], the expansion of word (or an empty string if
            // > word is omitted) shall be substituted; otherwise, the value of parameter
            // > shall be substituted.
            match expand_simple_parameter(parameter, interpreter) {
                Some(value) if !default_on_null || !value.is_empty() => value,
                _ => {
                    if let Some(default) = default {
                        expand_word_to_string(default, false, interpreter)
                    } else {
                        String::new()
                    }
                }
            }
        }
        ParameterExpansion::UnsetAssignDefault {
            parameter,
            word,
            assign_on_null,
        } => {
            // > If parameter is unset [or null], the expansion of word (or an empty string if
            // > word is omitted) shall be assigned to parameter. In all cases, the final value
            // > of parameter shall be substituted. Only variables, not positional parameters
            // > or special parameters, can be assigned in this way.
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
                    value
                }
            }
        }
        ParameterExpansion::UnsetError {
            parameter,
            word,
            error_on_null,
        } => {
            // > If parameter is unset [or null], the expansion of word (or a message indicating
            // > it is unset if word is omitted) shall be written to standard error and the
            // > shell exits with a non-zero exit status. Otherwise, the value of parameter
            // > shall be substituted. An interactive shell need not exit.
            match expand_simple_parameter(parameter, interpreter) {
                Some(value) if !error_on_null || !value.is_empty() => value,
                _ => {
                    if let Some(word) = word {
                        eprintln!("{}", expand_word_to_string(word, false, interpreter));
                    } else if *error_on_null {
                        eprintln!("parameter is unset or null");
                    } else {
                        eprintln!("parameter is unset");
                    }
                    std::process::exit(1);
                }
            }
        }
        ParameterExpansion::SetUseAlternative {
            parameter,
            word,
            substitute_null_with_word,
        } => {
            // > If parameter is unset [or null], null shall be substituted; otherwise, the
            // > expansion of word (or an empty string if word is omitted) shall be substituted.
            match expand_simple_parameter(parameter, interpreter) {
                Some(value) if *substitute_null_with_word || !value.is_empty() => {
                    expand_word_to_string(word.as_ref().unwrap(), false, interpreter)
                }
                _ => String::new(),
            }
        }
        ParameterExpansion::StrLen(parameter) => {
            // > The length in characters of the value of parameter shall be substituted.
            // > If parameter is '*' or '@', the result of the expansion is unspecified.
            // > If parameter is unset and set -u is in effect, the expansion shall fail.
            if matches!(
                parameter,
                Parameter::Special(SpecialParameter::Asterisk)
                    | Parameter::Special(SpecialParameter::At)
            ) {
                todo!("error: length of '*' or '@' is unspecified")
            }
            match expand_simple_parameter(parameter, interpreter) {
                Some(value) => value.len().to_string(),
                None => {
                    todo!("fail if set -u is in effect")
                }
            }
        }
        ParameterExpansion::RemovePattern {
            parameter,
            pattern,
            remove_prefix,
            remove_largest,
        } => {
            let param_str = expand_simple_parameter(parameter, interpreter).unwrap_or_default();
            if param_str.is_empty() {
                return String::new();
            }
            if let Some(word) = pattern {
                let expanded_word = expand_word_simple(word, false, interpreter);
                // TODO: fix unwrap
                let pattern = Pattern::new(&expanded_word).unwrap();
                if *remove_prefix {
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
                }
            } else {
                param_str
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

    #[test]
    fn expand_simple_named_parameter() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test_user")]);
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Variable("HOME".into())),
                &mut interpreter
            ),
            "/home/test_user"
        );
    }

    #[test]
    fn expand_special_parameters() {
        let mut interpreter = Interpreter::default();
        interpreter.shell_pid = 123;
        interpreter.most_recent_background_command_pid = 456;
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Dollar)),
                &mut interpreter
            ),
            "123"
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang)),
                &mut interpreter
            ),
            "456"
        );
    }

    #[test]
    fn unset_use_default_parameter_expansion() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test_user"), ("NULL", "")]);
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("HOME".into()),
                    word: None,
                    default_on_null: false,
                },
                &mut interpreter
            ),
            "/home/test_user"
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: Some(unquoted_literal("default")),
                    default_on_null: false,
                },
                &mut interpreter
            ),
            "default"
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetUseDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    default_on_null: false,
                },
                &mut interpreter
            ),
            ""
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetUseDefault {
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
            expand_parameter(
                &ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: Some(unquoted_literal("value")),
                    assign_on_null: false,
                },
                &mut interpreter
            ),
            "value"
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Variable("unset_var".into())),
                &mut interpreter
            ),
            "value"
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: None,
                    assign_on_null: false,
                },
                &mut interpreter
            ),
            ""
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Variable("unset_var".into())),
                &mut interpreter
            ),
            "value".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    assign_on_null: false,
                },
                &mut interpreter
            ),
            "default".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Variable("NULL".into())),
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::UnsetAssignDefault {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("default")),
                    assign_on_null: true,
                },
                &mut interpreter
            ),
            "default".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::Simple(Parameter::Variable("NULL".into())),
                &mut interpreter
            ),
            "default".to_string()
        );
    }

    #[test]
    fn set_use_alternative_parameter_expansion() {
        let mut interpreter = interpreter_with_env(&[("HOME", "/home/test"), ("NULL", "")]);
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("HOME".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: false,
                },
                &mut interpreter
            ),
            "word".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("unset_var".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::SetUseAlternative {
                    parameter: Parameter::Variable("NULL".into()),
                    word: Some(unquoted_literal("word")),
                    substitute_null_with_word: false,
                },
                &mut interpreter
            ),
            "".to_string()
        );
        assert_eq!(
            expand_parameter(
                &ParameterExpansion::SetUseAlternative {
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
            expand_parameter(
                &ParameterExpansion::StrLen(Parameter::Variable("HOME".into())),
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
            expand_parameter(
                &ParameterExpansion::RemovePattern {
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
}
