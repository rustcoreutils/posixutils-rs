//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::word::{Word, WordPart};
use crate::pattern::{FilenamePattern, Pattern};
use crate::shell::{CommandExecutionError, Shell};
use crate::wordexp::arithmetic::expand_arithmetic_expression_into;
use crate::wordexp::expanded_word::{ExpandedWord, ExpandedWordPart};
use crate::wordexp::parameter::expand_parameter_into;
use crate::wordexp::pathname::glob;
use crate::wordexp::tilde::tilde_expansion;
use std::path::Path;

mod arithmetic;
pub mod expanded_word;
mod parameter;
pub mod pathname;
mod tilde;

pub type ExpansionResult<T> = Result<T, CommandExecutionError>;

fn is_ifs_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\n'
}

fn split_generated_unquoted_literal(
    lit: String,
    last_word: &mut ExpandedWord,
    result: &mut Vec<ExpandedWord>,
    ifs: &str,
    max_fields: usize,
) {
    if lit.is_empty() {
        return;
    }
    let mut accumulator = String::new();
    let mut iter = lit.chars();
    let mut next_char = iter.next().unwrap();
    'outer: loop {
        if result.len() == max_fields - 1 {
            accumulator.push(next_char);
            accumulator.extend(iter);
            last_word.append(accumulator, false, false);
            return;
        }
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
            } else if let Some(c) = iter.next() {
                next_char = c;
            } else {
                break;
            }

            if !accumulator.is_empty() {
                last_word.append(accumulator, false, false);
                accumulator = String::new();
                let mut temp = ExpandedWord::default();
                std::mem::swap(&mut temp, last_word);
                result.push(temp);
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

fn insert_remaining_parts_into(
    word: &mut ExpandedWord,
    iter: impl Iterator<Item = ExpandedWordPart>,
) {
    for part in iter {
        match part {
            ExpandedWordPart::QuotedLiteral(val) => word.append(val, true, false),
            ExpandedWordPart::UnquotedLiteral(val) => word.append(val, false, false),
            ExpandedWordPart::GeneratedUnquotedLiteral(val) => word.append(val, false, true),
            ExpandedWordPart::FieldEnd => word.end_field(),
        }
    }
}

/// If there are more fields than `max_fields`, the remaining parts of `expanded_word`
/// will be put into the last entry of the result
/// # Panic
/// Panics if `max_fields` is 0
pub fn split_fields(
    expanded_word: ExpandedWord,
    ifs: Option<&str>,
    max_fields: usize,
) -> Vec<ExpandedWord> {
    assert_ne!(max_fields, 0);

    if expanded_word.is_empty() {
        return Vec::new();
    }

    let ifs = ifs.unwrap_or(" \t\n");
    if ifs.is_empty() {
        return vec![expanded_word];
    }

    let mut result = Vec::with_capacity(expanded_word.len());
    let mut last_word = ExpandedWord::default();
    let mut iter = expanded_word.into_iter();
    // we need the iterator later to insert the remaining parts into the last word
    // so we can't use a for loop
    #[allow(clippy::while_let_on_iterator)]
    while let Some(part) = iter.next() {
        match part {
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
                split_generated_unquoted_literal(lit, &mut last_word, &mut result, ifs, max_fields);
            }
        }
        if result.len() == max_fields {
            break;
        }
    }
    insert_remaining_parts_into(&mut last_word, iter);
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
                expand_parameter_into(result, &expansion, inside_double_quotes, true, shell)?;
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
    for field in split_fields(expanded_word, ifs, usize::MAX) {
        if shell.set_options.noglob {
            result.push(field.to_string())
        } else {
            let pattern =
                FilenamePattern::new(&field).map_err(CommandExecutionError::ExpansionError)?;
            let files = glob(&pattern, Path::new(&shell.current_directory));
            if files.is_empty() {
                result.push(pattern.into())
            } else {
                result.reserve(files.len());
                for file in files {
                    match file.into_string() {
                        Ok(string) => result.push(string),
                        Err(os_string) => {
                            return Err(CommandExecutionError::ExpansionError(format!(
                                "{} contains invalid utf8",
                                os_string.to_string_lossy()
                            )))
                        }
                    }
                }
            }
        }
    }
    Ok(result)
}

pub fn word_to_pattern(word: &Word, shell: &mut Shell) -> ExpansionResult<Pattern> {
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(&mut expanded_word, word, false, shell)?;
    Pattern::new(&expanded_word).map_err(CommandExecutionError::ExpansionError)
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn split_fields_on_empty_literal() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal(""),
                None,
                usize::MAX
            ),
            Vec::<ExpandedWord>::new()
        );
    }

    #[test]
    fn split_fields_on_single_non_whitespace_char() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a:b:c:"),
                Some(":"),
                usize::MAX
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
                Some(":/-"),
                usize::MAX
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
                Some(" "),
                usize::MAX
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
                Some("\t\n"),
                usize::MAX
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
                None,
                usize::MAX
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
                Some(",.:  \n"),
                usize::MAX
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
            split_fields(
                ExpandedWord::unquoted_literal("a:b:c"),
                Some(":"),
                usize::MAX
            ),
            vec![ExpandedWord::unquoted_literal("a:b:c")]
        );
        assert_eq!(
            split_fields(
                ExpandedWord::from_parts(vec![
                    ExpandedWordPart::UnquotedLiteral("a:".to_string()),
                    ExpandedWordPart::GeneratedUnquotedLiteral("b:c".to_string()),
                    ExpandedWordPart::UnquotedLiteral(":d".to_string())
                ]),
                Some(":"),
                usize::MAX
            ),
            vec![
                ExpandedWord::unquoted_literal("a:b"),
                ExpandedWord::unquoted_literal("c:d")
            ]
        );
    }

    #[test]
    fn split_fields_respects_max_fields() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a:b:c:d:e:f"),
                Some(":"),
                1
            ),
            vec![ExpandedWord::unquoted_literal("a:b:c:d:e:f")]
        );

        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("a:b:c:d:e:f"),
                Some(":"),
                3
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("b"),
                ExpandedWord::unquoted_literal("c:d:e:f")
            ]
        );

        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("one two three four five"),
                None,
                2
            ),
            vec![
                ExpandedWord::unquoted_literal("one"),
                ExpandedWord::unquoted_literal("two three four five")
            ]
        );
    }
}
