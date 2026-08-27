//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::shstr::ShString;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpandedWordPart {
    QuotedLiteral(ShString),
    UnquotedLiteral(ShString),
    GeneratedUnquotedLiteral(ShString),
    // terminates a field
    /// Ends a field regardless of IFS: `"$@"` separates its parameters even
    /// when they are empty and even when IFS is null.
    FieldEnd,
    /// Ends a field only if one has been accumulated. Unquoted `$@`/`$*` also
    /// separate their parameters, but POSIX 2.5.2 lets an empty one be
    /// discarded rather than producing an empty field.
    SoftFieldEnd,
}

impl ExpandedWordPart {
    pub fn new(value: ShString, quoted: bool, generated: bool) -> Self {
        if quoted {
            ExpandedWordPart::QuotedLiteral(value)
        } else if generated {
            ExpandedWordPart::GeneratedUnquotedLiteral(value)
        } else {
            ExpandedWordPart::UnquotedLiteral(value)
        }
    }
}

/// Word that has undergone:
/// - tilde expansion
/// - parameter expansion
/// - command substitution
/// - arithmetic expansion
///
/// Guarantees that adjacent parts are of different types and that the first element is not `ExpandedWordPart::FieldEnd`
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ExpandedWord {
    parts: Vec<ExpandedWordPart>,
    /// Set when a double-quoted `"$@"` in this word expanded to no positional
    /// parameters. POSIX 2.5.2: "If there are no positional parameters, the
    /// expansion of '@' shall generate zero fields, even when '@' is within
    /// double-quotes" — but the quotes around it still contribute an empty
    /// literal, so the emptiness has to be remembered rather than inferred.
    quoted_at_expanded_to_nothing: bool,
}

impl From<ExpandedWord> for ShString {
    fn from(value: ExpandedWord) -> Self {
        value.to_sh_string()
    }
}

impl ExpandedWord {
    /// The word's bytes with the field markers dropped. Deliberately not
    /// `Display`: the result is a *value*, and a lossy conversion here would
    /// corrupt it silently.
    pub fn to_sh_string(&self) -> ShString {
        let mut result = ShString::new();
        for p in &self.parts {
            match p {
                ExpandedWordPart::UnquotedLiteral(s)
                | ExpandedWordPart::QuotedLiteral(s)
                | ExpandedWordPart::GeneratedUnquotedLiteral(s) => result.push_bytes(s),
                ExpandedWordPart::FieldEnd | ExpandedWordPart::SoftFieldEnd => {}
            }
        }
        result
    }

    /// The word as bytes, for matching and for building a field.
    pub fn as_bytes_vec(&self) -> Vec<u8> {
        self.to_sh_string().into_bytes()
    }
}

impl IntoIterator for ExpandedWord {
    type Item = ExpandedWordPart;
    type IntoIter = std::vec::IntoIter<ExpandedWordPart>;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.into_iter()
    }
}

impl<'a> IntoIterator for &'a ExpandedWord {
    type Item = &'a ExpandedWordPart;
    type IntoIter = std::slice::Iter<'a, ExpandedWordPart>;

    fn into_iter(self) -> Self::IntoIter {
        self.parts.iter()
    }
}

impl ExpandedWord {
    pub fn unquoted_literal<S: Into<ShString>>(s: S) -> Self {
        Self {
            parts: vec![ExpandedWordPart::UnquotedLiteral(s.into())],
            quoted_at_expanded_to_nothing: false,
        }
    }

    pub fn append<S: AsRef<[u8]> + Into<ShString>>(
        &mut self,
        value: S,
        quoted: bool,
        generated: bool,
    ) {
        if let Some(last) = self.parts.last_mut() {
            match last {
                ExpandedWordPart::GeneratedUnquotedLiteral(last) if generated && !quoted => {
                    last.push_bytes(value.as_ref());
                }
                ExpandedWordPart::UnquotedLiteral(last) if !generated && !quoted => {
                    last.push_bytes(value.as_ref())
                }
                ExpandedWordPart::QuotedLiteral(last) if quoted => {
                    last.push_bytes(value.as_ref());
                }
                _ => self
                    .parts
                    .push(ExpandedWordPart::new(value.into(), quoted, generated)),
            }
        } else {
            self.parts
                .push(ExpandedWordPart::new(value.into(), quoted, generated));
        }
    }

    /// # Panics
    /// Panics if the last part is a field end or if the word is empty
    pub fn end_field(&mut self) {
        assert_ne!(self.parts.last(), Some(&ExpandedWordPart::FieldEnd));
        assert_ne!(self.parts.last(), Some(&ExpandedWordPart::SoftFieldEnd));
        assert!(!self.parts.is_empty());
        self.parts.push(ExpandedWordPart::FieldEnd);
    }

    /// Records that a double-quoted `"$@"` here had no positional parameters.
    pub fn note_quoted_at_expanded_to_nothing(&mut self) {
        self.quoted_at_expanded_to_nothing = true;
    }

    pub fn had_quoted_at_expanded_to_nothing(&self) -> bool {
        self.quoted_at_expanded_to_nothing
    }

    /// A field boundary that yields nothing when no field has accumulated.
    /// Unlike [`Self::end_field`] this may follow an empty word, since dropping
    /// the empty field is exactly its purpose.
    pub fn end_field_soft(&mut self) {
        if matches!(
            self.parts.last(),
            None | Some(ExpandedWordPart::FieldEnd) | Some(ExpandedWordPart::SoftFieldEnd)
        ) {
            return;
        }
        self.parts.push(ExpandedWordPart::SoftFieldEnd);
    }

    pub fn extend(&mut self, other: Self) {
        self.quoted_at_expanded_to_nothing |= other.quoted_at_expanded_to_nothing;
        self.parts.reserve(other.parts.len());
        let mut iter = other.parts.into_iter();
        if let Some(first) = iter.next() {
            if self.parts.is_empty() {
                self.parts.push(first);
            } else {
                match (first, self.parts.last_mut().unwrap()) {
                    (
                        ExpandedWordPart::UnquotedLiteral(lit),
                        ExpandedWordPart::UnquotedLiteral(dest),
                    ) => dest.push_bytes(&lit),
                    (
                        ExpandedWordPart::GeneratedUnquotedLiteral(lit),
                        ExpandedWordPart::GeneratedUnquotedLiteral(dest),
                    ) => dest.push_bytes(&lit),
                    (
                        ExpandedWordPart::QuotedLiteral(lit),
                        ExpandedWordPart::QuotedLiteral(dest),
                    ) => dest.push_bytes(&lit),
                    (part, _) => self.parts.push(part),
                }
            }
        }
        self.parts.extend(iter);
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    impl ExpandedWord {
        pub fn quoted_literal(s: &str) -> Self {
            Self {
                parts: vec![ExpandedWordPart::QuotedLiteral(ShString::from(s))],
                quoted_at_expanded_to_nothing: false,
            }
        }

        pub fn generated_unquoted_literal(s: &str) -> Self {
            Self {
                parts: vec![ExpandedWordPart::GeneratedUnquotedLiteral(ShString::from(
                    s,
                ))],
                quoted_at_expanded_to_nothing: false,
            }
        }

        pub fn from_parts(parts: Vec<ExpandedWordPart>) -> Self {
            Self {
                parts,
                quoted_at_expanded_to_nothing: false,
            }
        }
    }
}
