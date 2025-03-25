#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpandedWordPart {
    QuotedLiteral(String),
    UnquotedLiteral(String),
    GeneratedUnquotedLiteral(String),
    // terminates a field
    FieldEnd,
}

impl ExpandedWordPart {
    pub fn new(value: String, quoted: bool, generated: bool) -> Self {
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
}

impl From<ExpandedWord> for String {
    fn from(value: ExpandedWord) -> Self {
        value.to_string()
    }
}

impl ToString for ExpandedWord {
    fn to_string(&self) -> String {
        self.parts
            .iter()
            .map(|p| match p {
                ExpandedWordPart::UnquotedLiteral(s) => s.as_str(),
                ExpandedWordPart::QuotedLiteral(s) => s.as_str(),
                ExpandedWordPart::GeneratedUnquotedLiteral(s) => s.as_str(),
                ExpandedWordPart::FieldEnd => "",
            })
            .collect()
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
    pub fn unquoted_literal<S: Into<String>>(s: S) -> Self {
        Self {
            parts: vec![ExpandedWordPart::UnquotedLiteral(s.into())],
        }
    }

    pub fn append<S: AsRef<str> + Into<String>>(
        &mut self,
        value: S,
        quoted: bool,
        generated: bool,
    ) {
        if let Some(last) = self.parts.last_mut() {
            match last {
                ExpandedWordPart::GeneratedUnquotedLiteral(last) if generated && !quoted => {
                    last.push_str(value.as_ref());
                }
                ExpandedWordPart::UnquotedLiteral(last) if !generated && !quoted => {
                    last.push_str(value.as_ref())
                }
                ExpandedWordPart::QuotedLiteral(last) if quoted => {
                    last.push_str(value.as_ref());
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
    /// Panics if the last part is `ExpandedWordPart::FieldEnd` or if the word is empty
    pub fn end_field(&mut self) {
        assert_ne!(self.parts.last(), Some(&ExpandedWordPart::FieldEnd));
        assert!(!self.parts.is_empty());
        self.parts.push(ExpandedWordPart::FieldEnd);
    }

    pub fn extend(&mut self, other: Self) {
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
                    ) => dest.push_str(&lit),
                    (
                        ExpandedWordPart::GeneratedUnquotedLiteral(lit),
                        ExpandedWordPart::GeneratedUnquotedLiteral(dest),
                    ) => dest.push_str(&lit),
                    (
                        ExpandedWordPart::QuotedLiteral(lit),
                        ExpandedWordPart::QuotedLiteral(dest),
                    ) => dest.push_str(&lit),
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
                parts: vec![ExpandedWordPart::QuotedLiteral(s.to_string())],
            }
        }

        pub fn generated_unquoted_literal(s: &str) -> Self {
            Self {
                parts: vec![ExpandedWordPart::GeneratedUnquotedLiteral(s.to_string())],
            }
        }

        pub fn from_parts(parts: Vec<ExpandedWordPart>) -> Self {
            Self { parts }
        }
    }
}
