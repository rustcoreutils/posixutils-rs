//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::word::{Parameter, ParameterExpansion, SpecialParameter};
use crate::shell::{CommandExecutionError, Shell};
use crate::shstr::{CharOrByte, ShStr, ShString};
use crate::wordexp::tilde::TildeMode;
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
    str: Option<&ShStr>,
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

/// Joins the positional parameters with `separator`, byte-wise.
fn join_parameters(parameters: &[ShString], separator: &[u8]) -> ShString {
    let mut result = ShString::new();
    for (index, parameter) in parameters.iter().enumerate() {
        if index > 0 {
            result.push_bytes(separator);
        }
        result.push_bytes(parameter);
    }
    result
}

/// The separator `$*` joins with: the *first character* of IFS, or the first
/// byte when that byte is not part of one. Slicing one byte unconditionally
/// split a multi-byte character and panicked (`IFS=é`). An unset IFS behaves as
/// a space; an empty IFS joins with nothing.
fn dollar_star_separator(shell: &Shell) -> ShString {
    match shell.environment.get_value("IFS") {
        Some(ifs) => match ifs.chars_lossless().next() {
            Some(CharOrByte::Char(c)) => ShString::from(&ifs[..c.len_utf8()]),
            Some(CharOrByte::Byte(b)) => ShString::from(vec![b]),
            None => ShString::new(),
        },
        None => ShString::from(" "),
    }
}

fn add_split_parameters_to_expanded_word(
    word: &mut ExpandedWord,
    parameters: &[ShString],
    quoted: bool,
) {
    if parameters.is_empty() {
        return;
    }
    let mut i = 0;
    while i < parameters.len() - 1 {
        word.append(&parameters[i], quoted, true);
        // `"$@"` keeps a null parameter as an empty field; unquoted `$@` may
        // discard it (POSIX 2.5.2).
        if quoted {
            word.end_field();
        } else {
            word.end_field_soft();
        }
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
                .map(|s| s.as_sh_str()),
            inside_double_quotes,
        ),
        Parameter::Variable(var_name) => add_option_to_expanded_word(
            expanded_word,
            shell.environment.get_value(var_name.as_ref()),
            inside_double_quotes,
        ),
        Parameter::Special(special_parameter) => {
            match special_parameter {
                SpecialParameter::At => {
                    if inside_double_quotes && shell.positional_parameters.is_empty() {
                        // POSIX 2.5.2: `"$@"` with no positional parameters
                        // generates zero fields, not one empty field. The
                        // surrounding quotes still contribute an empty literal,
                        // so record it for the field splitter.
                        expanded_word.note_quoted_at_expanded_to_nothing();
                    }
                    if !field_splitting_will_be_performed {
                        expanded_word.append(
                            join_parameters(&shell.positional_parameters, b" "),
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
                        // POSIX: `$*` joins with the *first character* of IFS.
                        // Slicing one byte split a multi-byte character and
                        // panicked (`IFS=é`); IFS may also hold bytes that are
                        // not characters at all, and the first of those is
                        // still the separator.
                        let separator = match shell.environment.get_value("IFS") {
                            Some(ifs) => match ifs.chars_lossless().next() {
                                Some(CharOrByte::Char(c)) => ShString::from(&ifs[..c.len_utf8()]),
                                Some(CharOrByte::Byte(b)) => ShString::from(vec![b]),
                                None => ShString::new(),
                            },
                            None => ShString::from(" "),
                        };
                        expanded_word.append(
                            join_parameters(&shell.positional_parameters, separator.as_bytes()),
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
                        shell.last_pipeline_exit_status.to_string(),
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
                        .last_background_pid
                        .map(|pid| pid.to_string())
                        .unwrap_or_default(),
                    inside_double_quotes,
                    true,
                ),
                SpecialParameter::Zero => {
                    expanded_word.append(shell.program_name.clone(), inside_double_quotes, true);
                }
            }
            // `$@` and `$*` follow the positional parameters: with none set
            // they are unset, and `${*:-word}` must substitute. The rest are
            // always set.
            match special_parameter {
                SpecialParameter::At | SpecialParameter::Asterisk => {
                    // Always set, but null when the *joined* value is empty, so
                    // `${*:-word}` substitutes with no positional parameters
                    // while `${*-word}` does not. The separators count: two
                    // empty parameters join to a single space and so are not
                    // null, unless IFS is itself empty.
                    let all_empty = shell.positional_parameters.iter().all(|p| p.is_empty());
                    let joins_to_nothing = shell.positional_parameters.len() < 2
                        || dollar_star_separator(shell).is_empty();
                    if all_empty && joins_to_nothing {
                        ParameterExpansionResult::Null
                    } else {
                        ParameterExpansionResult::Set
                    }
                }
                _ => ParameterExpansionResult::Set,
            }
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
            let result = expand_simple_parameter_into(
                expanded_word,
                parameter,
                inside_double_quotes,
                field_splitting_will_be_performed,
                shell,
            );
            // `set -u`: expanding an unset variable or positional parameter is an
            // error (special parameters are always considered set).
            if shell.set_options.nounset && result.is_unset() {
                let name = match parameter {
                    Parameter::Number(n) => n.to_string(),
                    Parameter::Variable(var) => var.as_ref().to_string(),
                    Parameter::Special(_) => String::new(),
                };
                return Err(CommandExecutionError::ExpansionError(format!(
                    "{name}: parameter not set"
                )));
            }
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
                // The default *replaces* the parameter, so its own expansion
                // must not be appended as well.
                simple_word_expansion_into(expanded_word, default, TildeMode::Word, shell)?;
            } else {
                expanded_word.extend(expanded_parameter);
            }
        }
        ParameterExpansion::UnsetAssignDefault {
            variable: variable_name,
            word,
            assign_on_null,
        } => {
            // POSIX: if the substitution is not needed, `word` shall NOT be
            // expanded (it may have side effects), so decide first, expand last.
            let needs_assign = match shell.environment.get_str_value(variable_name) {
                None => true,
                Some(current) => current.is_empty() && *assign_on_null,
            };
            if needs_assign {
                let value = expand_word_to_string(word, false, shell)?;
                shell.assign_global(variable_name.to_string(), value.clone())?;
                expanded_word.append(value, inside_double_quotes, true);
            } else {
                let current = shell
                    .environment
                    .get_str_value(variable_name)
                    .unwrap_or_default()
                    .to_string();
                expanded_word.append(current, inside_double_quotes, true);
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
                // POSIX: if `word` is supplied, expand it and use it as the
                // diagnostic; otherwise emit a default "unset"/"null" message.
                let message = if word.parts.is_empty() {
                    if *error_on_null {
                        "parameter null or not set".to_string()
                    } else {
                        "parameter not set".to_string()
                    }
                } else {
                    // A diagnostic, so a lossy view is the right conversion.
                    expand_word_to_string(word, false, shell)?
                        .display()
                        .to_string()
                };
                return Err(CommandExecutionError::ExpansionError(message));
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
                simple_word_expansion_into(expanded_word, word, TildeMode::Word, shell)?
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
            // POSIX: length in characters, not bytes.
            expanded_word.append(
                expanded_parameter
                    .to_sh_string()
                    .chars_lossless()
                    .count()
                    .to_string(),
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
            let param_str = expanded_parameter.to_sh_string();

            let pattern = word_to_pattern(pattern, shell)?;
            let result = if *remove_prefix {
                if *remove_largest {
                    pattern.remove_largest_prefix(param_str)
                } else {
                    pattern.remove_shortest_prefix(param_str)
                }
            } else if *remove_largest {
                pattern.remove_largest_suffix(param_str)
            } else {
                pattern.remove_shortest_suffix(param_str)
            };
            expanded_word.append(result, inside_double_quotes, true);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jobs::JobState;
    use crate::parse::word::test_utils::unquoted_literal;
    use crate::parse::word::Word;
    use crate::shstr::ShString;
    use crate::wordexp::expanded_word::ExpandedWordPart;

    fn shell_with_env(env: &[(&str, &str)]) -> Shell {
        let mut shell = Shell::default();
        for (k, v) in env {
            shell
                .environment
                .set_global(k.to_string(), v.to_string())
                .expect("failed to set var");
        }
        shell
    }

    fn shell_with_positional_arguments(args: Vec<&str>) -> Shell {
        Shell {
            positional_parameters: args.iter().map(|s| ShString::from(*s)).collect(),
            ..Default::default()
        }
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
        )
        .unwrap();
        String::from_utf8(expanded_word.as_bytes_vec()).unwrap()
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
        )
        .unwrap();
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
        let mut shell = Shell {
            shell_pid: 123,
            ..Default::default()
        };
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
        // `$!` is latched when an asynchronous command is started and, unlike
        // the job table, survives the job being waited for.
        shell.last_background_pid = Some(123);
        shell
            .background_jobs
            .add_job(123, "cmd".to_string(), JobState::Running);
        assert_eq!(
            expand_parameter_to_string(
                ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang)),
                &mut shell
            ),
            "123".to_string()
        );
        shell.background_jobs.current_mut().unwrap().state = JobState::Done(0);
        shell.background_jobs.collect_terminated_jobs();
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
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg1")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg2")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg3"))
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
                ExpandedWordPart::QuotedLiteral(ShString::from("arg1")),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::QuotedLiteral(ShString::from("arg2")),
                ExpandedWordPart::FieldEnd,
                ExpandedWordPart::QuotedLiteral(ShString::from("arg3"))
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
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg1")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg2")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg3"))
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
            .set_global("IFS".to_string(), "".to_string())
            .unwrap();
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
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg1")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg2")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg3"))
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
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg1")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg2")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg3"))
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
            .set_global("IFS".to_string(), ",:".to_string())
            .unwrap();
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
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg1")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg2")),
                ExpandedWordPart::SoftFieldEnd,
                ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("arg3"))
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
