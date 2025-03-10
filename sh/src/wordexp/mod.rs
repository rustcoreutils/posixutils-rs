use crate::parse::word::{Word, WordPart};
use crate::shell::environment::CannotModifyReadonly;
use crate::shell::{CommandExecutionError, Shell};
use crate::utils::OsError;
use crate::wordexp::arithmetic::expand_arithmetic_expression_into;
use crate::wordexp::expanded_word::{ExpandedWord, ExpandedWordPart};
use crate::wordexp::parameter::expand_parameter_into;
use crate::wordexp::pathname::glob;
use crate::wordexp::pattern::{FilenamePattern, Pattern};
use crate::wordexp::tilde::tilde_expansion;
use std::path::Path;

mod arithmetic;
pub mod expanded_word;
mod parameter;
pub mod pathname;
pub mod pattern;
mod tilde;

pub type ExpansionResult<T> = Result<T, CommandExecutionError>;

fn is_ifs_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n'
}

fn split_fields(expanded_word: ExpandedWord, ifs: Option<&str>) -> Vec<ExpandedWord> {
    // TODO: look into "Note that the shell processes arbitrary bytes from the input fields;
    // there is no requirement that those bytes form valid characters."
    if expanded_word.is_empty() {
        return Vec::new();
    }
    // > If the IFS variable is unset [..], its value shall be considered to contain the three
    // > single-byte characters <space>, <tab>, and <newline>
    let ifs = ifs.unwrap_or(" \t\n");
    if ifs.is_empty() {
        // > If the IFS variable is set and has an empty string as its value, no field
        // > splitting shall occur
        return vec![expanded_word];
    }

    let mut result = Vec::with_capacity(expanded_word.len());
    let mut last_word = ExpandedWord::default();
    for part in expanded_word.into_iter() {
        match part {
            // > Fields which contain no results from expansions shall not be affected by
            // > field splitting, and shall remain unaltered, simply moving from the list
            // > of input fields to be next in the list of output fields.
            ExpandedWordPart::UnquotedLiteral(lit) => {
                last_word.append(lit, false, false);
            }
            ExpandedWordPart::QuotedLiteral(lit) => {
                last_word.append(lit, true, false);
            }
            ExpandedWordPart::FieldEnd => {
                result.push(last_word);
                last_word = ExpandedWord::default();
            }
            ExpandedWordPart::GeneratedUnquotedLiteral(lit) => {
                if lit.is_empty() {
                    continue;
                }
                let mut accumulator = String::new();
                // TODO: this could probably be done more cleanly
                let mut iter = lit.chars();
                let mut next_char = iter.next().unwrap();
                'outer: loop {
                    if ifs.contains(next_char) {
                        if is_ifs_whitespace(next_char) {
                            loop {
                                match iter.next() {
                                    Some(c) => {
                                        next_char = c;
                                        if !is_ifs_whitespace(next_char) {
                                            break;
                                        }
                                    }
                                    None => break 'outer,
                                }
                            }
                        } else {
                            if let Some(c) = iter.next() {
                                next_char = c;
                            } else {
                                break;
                            }
                        }
                        if !accumulator.is_empty() {
                            last_word.append(accumulator, false, false);
                            accumulator = String::new();
                            result.push(last_word);
                            last_word = ExpandedWord::default();
                        }
                    } else {
                        accumulator.push(next_char);
                        if let Some(c) = iter.next() {
                            next_char = c;
                        } else {
                            break;
                        }
                    }
                }
                if !accumulator.is_empty() {
                    last_word.append(accumulator, false, false);
                }
            }
        }
    }
    if !last_word.is_empty() {
        result.push(last_word);
    }
    result
}

/// performs:
/// - tilde expansion
/// - parameter expansion
/// - command substitution
/// - arithmetic expansion
fn simple_word_expansion_into(
    result: &mut ExpandedWord,
    word: &Word,
    is_assignment: bool,
    shell: &mut Shell,
) -> ExpansionResult<()> {
    let mut word = word.clone();
    tilde_expansion(&mut word, is_assignment, &shell.environment)
        .map_err(CommandExecutionError::ExpansionError)?;
    for part in word.parts.into_iter() {
        match part {
            WordPart::UnquotedLiteral(lit) => result.append(lit, false, false),
            WordPart::QuotedLiteral(lit) => result.append(lit, true, false),
            WordPart::ParameterExpansion {
                expansion,
                inside_double_quotes,
            } => {
                expand_parameter_into(result, &expansion, inside_double_quotes, true, shell);
            }
            WordPart::ArithmeticExpansion {
                expr,
                inside_double_quotes,
            } => expand_arithmetic_expression_into(result, &expr, inside_double_quotes, shell)?,
            WordPart::CommandSubstitution {
                commands,
                inside_double_quotes,
            } => {
                let output = shell.execute_in_subshell(&commands)?;
                result.append(output, inside_double_quotes, true);
            }
        }
    }
    Ok(())
}

/// performs:
/// - tilde expansion
/// - parameter expansion
/// - command substitution
/// - arithmetic expansion
pub fn expand_word_to_string(
    word: &Word,
    is_assignment: bool,
    shell: &mut Shell,
) -> ExpansionResult<String> {
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(&mut expanded_word, word, is_assignment, shell)?;
    Ok(expanded_word.to_string())
}

/// performs general word expansion (similar to `wordexp` from libc)
pub fn expand_word(
    word: &Word,
    is_assignment: bool,
    shell: &mut Shell,
) -> ExpansionResult<Vec<String>> {
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(&mut expanded_word, word, is_assignment, shell)?;
    let ifs = shell.environment.get_str_value("IFS");
    let mut result = Vec::new();
    for field in split_fields(expanded_word, ifs) {
        let pattern =
            FilenamePattern::new(&field).map_err(CommandExecutionError::ExpansionError)?;
        let files = glob(&pattern, Path::new(&shell.current_directory));
        if files.is_empty() {
            result.push(pattern.into())
        } else {
            // TODO: handle error
            result.extend(files.into_iter().map(|s| s.into_string().unwrap()))
        }
    }
    Ok(result)
}

pub fn word_to_pattern(word: &Word, shell: &mut Shell) -> ExpansionResult<Pattern> {
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(&mut expanded_word, &word, false, shell)?;
    Pattern::new(&expanded_word).map_err(CommandExecutionError::ExpansionError)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn split_fields_on_empty_literal() {
        assert_eq!(
            split_fields(ExpandedWord::generated_unquoted_literal(""), None),
            Vec::<ExpandedWord>::new()
        );
    }

    #[test]
    fn split_fields_on_single_non_whitespace_char() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a:b:c:"),
                Some(":")
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("b"),
                ExpandedWord::unquoted_literal("c")
            ]
        );
    }

    #[test]
    fn split_fields_on_multiple_non_whitespace_char() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a:b/c-d-:/x y"),
                Some(":/-")
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("b"),
                ExpandedWord::unquoted_literal("c"),
                ExpandedWord::unquoted_literal("d"),
                ExpandedWord::unquoted_literal("x y")
            ]
        );
    }

    #[test]
    fn split_fields_on_single_whitespace_char() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a b c "),
                Some(" ")
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("b"),
                ExpandedWord::unquoted_literal("c"),
            ]
        );
    }

    #[test]
    fn split_fields_on_multiple_whitespace_char() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("  a\t\n\t\nb  \tc\nd  e f"),
                Some("\t\n")
            ),
            vec![
                ExpandedWord::unquoted_literal("  a"),
                ExpandedWord::unquoted_literal("b  "),
                ExpandedWord::unquoted_literal("c"),
                ExpandedWord::unquoted_literal("d  e f"),
            ]
        )
    }

    #[test]
    fn split_fields_default_ifs() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal(
                    "\t\n   a\n\n\t  \n\t word,and\n\t\n \n\tc\n\n\n   \t\n\t "
                ),
                None
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("word,and"),
                ExpandedWord::unquoted_literal("c")
            ]
        );
    }

    #[test]
    fn split_fields_by_mixed_ifs() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a,b.c  d\n\ne  f"),
                Some(",.:  \n")
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("b"),
                ExpandedWord::unquoted_literal("c"),
                ExpandedWord::unquoted_literal("d"),
                ExpandedWord::unquoted_literal("e"),
                ExpandedWord::unquoted_literal("f")
            ]
        );
    }

    #[test]
    fn field_splitting_does_not_affect_non_generated_literals() {
        assert_eq!(
            split_fields(ExpandedWord::unquoted_literal("a:b:c"), Some(":")),
            vec![ExpandedWord::unquoted_literal("a:b:c")]
        );
        assert_eq!(
            split_fields(
                ExpandedWord::from_parts(vec![
                    ExpandedWordPart::UnquotedLiteral("a:".to_string()),
                    ExpandedWordPart::GeneratedUnquotedLiteral("b:c".to_string()),
                    ExpandedWordPart::UnquotedLiteral(":d".to_string())
                ]),
                Some(":")
            ),
            vec![
                ExpandedWord::unquoted_literal("a:b"),
                ExpandedWord::unquoted_literal("c:d")
            ]
        );
    }
}
