//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::word::{Word, WordPart};
use crate::pattern::{FilenamePattern, Pattern};
use crate::shell::{CommandExecutionError, Shell};
use crate::shstr::{CharOrByte, ShString};
use crate::wordexp::arithmetic::expand_arithmetic_expression_into;
use crate::wordexp::expanded_word::{ExpandedWord, ExpandedWordPart};
use crate::wordexp::parameter::expand_parameter_into;
use crate::wordexp::pathname::glob;
use crate::wordexp::tilde::{tilde_expansion, TildeMode};
use std::path::Path;

mod arithmetic;
pub mod expanded_word;
mod parameter;
pub mod pathname;
mod tilde;

pub type ExpansionResult<T> = Result<T, CommandExecutionError>;

/// SCAFFOLD(byte-core stage 1): the expansion pipeline now carries bytes, but
/// the shell around it still holds `String`. Converting here keeps the two
/// halves compiling without introducing any loss the crate did not already
/// have — this path already refused a glob result that was not valid text.
/// Removed when `Shell` and the builtins take bytes.
pub(crate) fn sh_string_to_string(value: ShString) -> ExpansionResult<String> {
    String::from_utf8(value.clone().into_bytes()).map_err(|_| {
        CommandExecutionError::ExpansionError(format!("{} contains invalid utf8", value.display()))
    })
}

/// Field splitting, POSIX XCU 2.6.5.
///
/// The standard describes a single pass over the bytes of the expanded word,
/// distinguishing bytes that came from an expansion (which may delimit fields)
/// from bytes that did not (which never do). `ExpandedWord` already records
/// that distinction, so the algorithm is transcribed directly rather than
/// spread over the callers.
///
/// The subtle part is the delimiter run. A run of IFS *white space* is one
/// delimiter, and it may additionally absorb a single non-white-space IFS
/// character; a non-white-space IFS character on its own always delimits, which
/// is what makes `IFS=:` turn `a::b` into three fields while `IFS=' :'` turns
/// `a : b` into two.
struct FieldSplitter<'a> {
    ifs: &'a str,
    max_fields: usize,
    result: Vec<ExpandedWord>,
    current: ExpandedWord,
    /// Inside a run of IFS characters that has not yet been resolved.
    in_delimiter_run: bool,
}

impl<'a> FieldSplitter<'a> {
    fn new(ifs: &'a str, max_fields: usize) -> Self {
        Self {
            ifs,
            max_fields,
            result: Vec::new(),
            current: ExpandedWord::default(),
            in_delimiter_run: false,
        }
    }

    fn is_ifs_whitespace(&self, c: char) -> bool {
        // POSIX names <space>, <tab> and <newline> as IFS white space whenever
        // they appear in IFS.
        matches!(c, ' ' | '\t' | '\n') && self.ifs.contains(c)
    }

    fn is_ifs(&self, c: char) -> bool {
        self.ifs.contains(c)
    }

    /// True once no further splitting may happen and everything left belongs to
    /// the final field.
    fn reached_max_fields(&self) -> bool {
        self.result.len() + 1 == self.max_fields
    }

    fn delimit(&mut self) {
        let field = std::mem::take(&mut self.current);
        self.result.push(field);
    }

    /// Resolve a pending delimiter run. `saw_non_whitespace` records whether it
    /// contained an IFS character that is not white space.
    fn end_delimiter_run(&mut self, saw_non_whitespace: bool) {
        if !self.in_delimiter_run {
            return;
        }
        self.in_delimiter_run = false;
        if !self.current.is_empty() || saw_non_whitespace {
            self.delimit();
        }
    }

    fn push_literal(&mut self, value: ShString, quoted: bool) {
        // Bytes that did not come from an expansion never delimit, but they do
        // end any run that was still open.
        self.end_delimiter_run(false);
        self.current.append(value, quoted, false);
    }

    fn push_generated(&mut self, value: ShString) {
        let mut accumulator = ShString::new();
        for element in value.chars_lossless() {
            // Only a character can be an IFS delimiter; a byte that is not part
            // of one is ordinary text.
            let delimiter = match element {
                CharOrByte::Char(c) => self.is_ifs(c) && !self.reached_max_fields(),
                CharOrByte::Byte(_) => false,
            };
            if delimiter {
                let CharOrByte::Char(c) = element else {
                    unreachable!("only a character can be a delimiter")
                };
                // Everything read so far belongs to the field being delimited.
                if !accumulator.is_empty() {
                    self.current
                        .append(std::mem::take(&mut accumulator), false, false);
                }
                self.in_delimiter_run = true;
                if !self.is_ifs_whitespace(c) {
                    // A non-white-space IFS character always delimits, and ends
                    // the run it belongs to.
                    self.end_delimiter_run(true);
                }
            } else {
                // An ordinary element ends any run without delimiting on its
                // own account.
                self.end_delimiter_run(false);
                match element {
                    CharOrByte::Char(c) => accumulator.push_char(c),
                    CharOrByte::Byte(b) => accumulator.push_bytes([b]),
                }
            }
        }
        if !accumulator.is_empty() {
            // Splitting has consumed the "came from an expansion" property, and
            // nothing downstream distinguishes the two kinds.
            self.current.append(accumulator, false, false);
        }
    }

    /// `"$@"` plants an explicit boundary that IFS has no say over.
    fn end_field(&mut self) {
        self.end_delimiter_run(false);
        self.delimit();
    }

    /// Unquoted `$@`/`$*` separate their parameters, but a parameter that
    /// contributes nothing is discarded rather than yielding an empty field.
    fn end_field_soft(&mut self) {
        self.end_delimiter_run(false);
        if !self.current.is_empty() {
            self.delimit();
        }
    }

    fn finish(mut self) -> Vec<ExpandedWord> {
        // POSIX: once the input is empty the candidate becomes a field only if
        // it is not empty. A run still open here delimits nothing further.
        self.in_delimiter_run = false;
        if !self.current.is_empty() {
            self.delimit();
        }
        self.result
    }
}

/// Splits `expanded_word` into fields according to POSIX XCU 2.6.5.
///
/// If there are more fields than `max_fields`, everything left goes into the
/// last entry of the result.
///
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

    // An unset IFS behaves as <space><tab><newline>. An IFS set to the empty
    // string suppresses splitting, but it has no bearing on the field
    // boundaries `"$@"` plants, which is why those are still honoured below.
    let ifs = ifs.unwrap_or(" \t\n");
    let quoted_at_expanded_to_nothing = expanded_word.had_quoted_at_expanded_to_nothing();
    let mut splitter = FieldSplitter::new(ifs, max_fields);
    for part in expanded_word {
        match part {
            ExpandedWordPart::UnquotedLiteral(lit) => splitter.push_literal(lit, false),
            ExpandedWordPart::QuotedLiteral(lit) => splitter.push_literal(lit, true),
            ExpandedWordPart::GeneratedUnquotedLiteral(lit) => {
                if ifs.is_empty() {
                    splitter.push_literal(lit, false)
                } else {
                    splitter.push_generated(lit)
                }
            }
            ExpandedWordPart::FieldEnd => splitter.end_field(),
            ExpandedWordPart::SoftFieldEnd => splitter.end_field_soft(),
        }
    }
    let fields = splitter.finish();
    // A word that is nothing but a `"$@"` with no parameters yields no fields;
    // the sole field here is the one its own quotes contributed.
    if quoted_at_expanded_to_nothing && fields.len() == 1 && fields[0].to_sh_string().is_empty() {
        return Vec::new();
    }
    fields
}

/// performs:
/// - tilde expansion
/// - parameter expansion
/// - command substitution
/// - arithmetic expansion
fn simple_word_expansion_into(
    result: &mut ExpandedWord,
    word: &Word,
    tilde_mode: TildeMode,
    shell: &mut Shell,
) -> ExpansionResult<()> {
    let mut word = word.clone();
    tilde_expansion(&mut word, tilde_mode, &shell.environment)
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
    let tilde_mode = if is_assignment {
        TildeMode::AssignmentValue
    } else {
        TildeMode::Word
    };
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(&mut expanded_word, word, tilde_mode, shell)?;
    // SCAFFOLD(byte-core stage 1): removed once the shell's values are bytes.
    // Errors rather than losing bytes, which is what this path already did.
    sh_string_to_string(expanded_word.to_sh_string())
}

/// Expands a `name=value` operand of a declaration utility (POSIX 2.9.1): the
/// value gets the tilde expansion of an assignment, and the result is a single
/// field (no field splitting, no pathname expansion).
pub fn expand_declaration_operand(word: &Word, shell: &mut Shell) -> ExpansionResult<String> {
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(
        &mut expanded_word,
        word,
        TildeMode::DeclarationOperand,
        shell,
    )?;
    // SCAFFOLD(byte-core stage 1): removed once the shell's values are bytes.
    // Errors rather than losing bytes, which is what this path already did.
    sh_string_to_string(expanded_word.to_sh_string())
}

/// performs general word expansion (similar to `wordexp` from libc)
pub fn expand_word(
    word: &Word,
    is_assignment: bool,
    shell: &mut Shell,
) -> ExpansionResult<Vec<String>> {
    let tilde_mode = if is_assignment {
        TildeMode::AssignmentValue
    } else {
        TildeMode::Word
    };
    let mut expanded_word = ExpandedWord::default();
    simple_word_expansion_into(&mut expanded_word, word, tilde_mode, shell)?;
    let ifs = shell.environment.get_str_value("IFS");
    let mut result = Vec::new();
    for field in split_fields(expanded_word, ifs, usize::MAX) {
        if shell.set_options.noglob {
            result.push(sh_string_to_string(field.to_sh_string())?)
        } else {
            let pattern =
                FilenamePattern::new(&field).map_err(CommandExecutionError::ExpansionError)?;
            let files = glob(&pattern, Path::new(&shell.current_directory));
            if files.is_empty() {
                result.push(sh_string_to_string(pattern.into())?)
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
    simple_word_expansion_into(&mut expanded_word, word, TildeMode::Word, shell)?;
    Pattern::new(&expanded_word).map_err(CommandExecutionError::ExpansionError)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::shstr::ShString;

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
    fn split_fields_on_whitespace_only_literal() {
        assert_eq!(
            split_fields(
                ExpandedWord::generated_unquoted_literal("   \t\n\n"),
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
                ExpandedWord::generated_unquoted_literal("a:b:c::"),
                Some(":"),
                usize::MAX
            ),
            vec![
                ExpandedWord::unquoted_literal("a"),
                ExpandedWord::unquoted_literal("b"),
                ExpandedWord::unquoted_literal("c"),
                // An empty field carries no parts at all, rather than one part
                // holding the empty string; both render as an empty field and
                // nothing downstream distinguishes them.
                ExpandedWord::default(),
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
                ExpandedWord::default(),
                ExpandedWord::default(),
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
                    ExpandedWordPart::UnquotedLiteral(ShString::from("a:")),
                    ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from("b:c")),
                    ExpandedWordPart::UnquotedLiteral(ShString::from(":d"))
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
