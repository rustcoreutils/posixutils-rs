use crate::pattern::parse::{parse_pattern, PatternItem};
use crate::pattern::regex::{parsed_pattern_to_regex, Regex};
use crate::wordexp::expanded_word::ExpandedWord;
use std::ffi::{CStr, CString};

mod parse;
mod regex;

pub struct Pattern {
    pattern_string: String,
    regex: Regex,
}

impl Pattern {
    pub fn new(word: &ExpandedWord) -> Result<Self, String> {
        let parsed_pattern = parse_pattern(word, false)?;
        let regex = parsed_pattern_to_regex(&parsed_pattern)?;
        Ok(Self {
            pattern_string: word.to_string(),
            regex,
        })
    }

    pub fn matches(&self, s: &CStr) -> bool {
        self.regex.matches(s)
    }

    pub fn remove_largest_prefix(&self, s: String) -> String {
        if self.pattern_string.is_empty() || s.is_empty() {
            return s;
        }
        let cstring = CString::new(s).expect("trying to match a string containing null");
        let mut prefix_end = 0;
        if let Some(regex_match) = self.regex.match_locations(&cstring).next() {
            if regex_match.start == 0 {
                prefix_end = regex_match.end;
            }
        }
        let mut bytes = cstring.into_bytes();
        bytes.drain(..prefix_end);
        String::from_utf8(bytes).expect("failed to create string after removing largest prefix")
    }

    pub fn remove_shortest_prefix(&self, s: String) -> String {
        if self.pattern_string.is_empty() || s.is_empty() {
            return s;
        }
        assert!(
            !s.as_bytes().contains(&b'\0'),
            "trying to match a string containing null"
        );
        let mut bytes = s.into_bytes();
        bytes.push(b'\0');
        let mut prefix_end = 0;
        for i in 1..bytes.len() - 1 {
            let t = bytes[i];
            bytes[i] = b'\0';
            // we know there is a null, so this unwrap will never fail
            if self
                .regex
                .matches(CStr::from_bytes_until_nul(&bytes).unwrap())
            {
                prefix_end = i;
                bytes[i] = t;
                break;
            }
            bytes[i] = t;
        }
        // remove '\0'
        bytes.pop();
        bytes.drain(..prefix_end);
        String::from_utf8(bytes).expect("failed to create string after removing shortest prefix")
    }

    pub fn remove_largest_suffix(&self, s: String) -> String {
        if self.pattern_string.is_empty() || s.is_empty() {
            return s;
        }
        let cstring = CString::new(s).expect("trying to match a string containing null");
        let len = cstring.as_bytes().len();
        let mut suffix_start = len - 1;
        for regex_match in self.regex.match_locations(&cstring) {
            if regex_match.end == len {
                suffix_start = regex_match.start;
                break;
            }
        }
        let mut bytes = cstring.into_bytes();
        bytes.drain(suffix_start..);
        String::from_utf8(bytes).expect("failed to create string after removing largest suffix")
    }

    pub fn remove_shortest_suffix(&self, s: String) -> String {
        if self.pattern_string.is_empty() || s.is_empty() {
            return s;
        }
        assert!(
            !s.as_bytes().contains(&b'\0'),
            "trying to match a string containing null"
        );
        let mut bytes = s.into_bytes();
        bytes.push(b'\0');
        let mut suffix_start = bytes.len();
        for i in (1..bytes.len() - 1).rev() {
            // we know there is a null, so this unwrap will never fail
            if self
                .regex
                .matches(CStr::from_bytes_until_nul(&bytes[i..]).unwrap())
            {
                suffix_start = i;
                break;
            }
        }
        // remove terminating '\0'
        bytes.pop();
        bytes.drain(suffix_start..);
        String::from_utf8(bytes).expect("failed to create string after removing shortest suffix")
    }
}

impl From<Pattern> for String {
    fn from(value: Pattern) -> Self {
        value.pattern_string
    }
}

struct FilenamePatternPart {
    regex: Regex,
    starts_with_dot: bool,
}

pub struct FilenamePattern {
    path_parts: Vec<FilenamePatternPart>,
    pattern_string: String,
}

impl FilenamePattern {
    pub fn new(word: &ExpandedWord) -> Result<Self, String> {
        let pattern_string = word.to_string();
        let parsed_pattern = parse_pattern(word, true)?;
        let mut path_parts = Vec::new();

        parsed_pattern
            .split(|item| *item == PatternItem::Char('/'))
            .filter(|items| !items.is_empty())
            .try_for_each(|items| {
                let starts_with_dot = items.starts_with(&[PatternItem::Char('.')]);
                let regex = parsed_pattern_to_regex(items)?;
                path_parts.push(FilenamePatternPart {
                    regex,
                    starts_with_dot,
                });
                Ok::<(), String>(())
            })?;

        Ok(Self {
            path_parts,
            pattern_string,
        })
    }

    /// # Panics
    /// panics if `depth` is smaller than 1 or bigger than `component_count`
    pub fn matches_all(&self, depth: usize, s: &CStr) -> bool {
        assert!(
            depth > 0 && depth <= self.component_count(),
            "invalid depth"
        );
        let component_index = depth - 1;
        if s.to_bytes()[0] == b'.' && !self.path_parts[component_index].starts_with_dot {
            // dot at the start is only matched explicitly
            return false;
        }
        if let Some(loc) = self.path_parts[component_index]
            .regex
            .match_locations(s)
            .next()
        {
            loc.start == 0 && loc.end == s.count_bytes()
        } else {
            false
        }
    }

    /// Returns number of components in the path
    /// If it returns 0 then the pattern is just a directory (root if it starts
    /// with '/', the current directory otherwise)
    pub fn component_count(&self) -> usize {
        self.path_parts.len()
    }

    pub fn is_absolute(&self) -> bool {
        self.pattern_string.starts_with('/')
    }
}

impl From<FilenamePattern> for String {
    fn from(value: FilenamePattern) -> Self {
        value.pattern_string
    }
}

impl TryFrom<String> for FilenamePattern {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let value = ExpandedWord::unquoted_literal(value);
        FilenamePattern::new(&value)
    }
}

pub struct HistoryPattern {
    regex: Regex,
    match_only_at_line_start: bool,
}

impl HistoryPattern {
    pub fn new(pattern: String) -> Result<Option<Self>, ()> {
        let parsed_pattern =
            parse_pattern(&ExpandedWord::unquoted_literal(pattern), false).map_err(|_| ())?;
        if parsed_pattern.is_empty() {
            return Ok(None);
        }
        if parsed_pattern[0] == PatternItem::Char('^') {
            let regex = parsed_pattern_to_regex(&parsed_pattern[1..]).map_err(|_| ())?;
            Ok(Some(Self {
                regex,
                match_only_at_line_start: true,
            }))
        } else {
            let regex = parsed_pattern_to_regex(&parsed_pattern).map_err(|_| ())?;
            Ok(Some(Self {
                regex,
                match_only_at_line_start: false,
            }))
        }
    }

    pub fn matches(&self, s: &str) -> bool {
        if let Ok(s_cstr) = CString::new(s) {
            if let Some(first_match) = self.regex.match_locations(&s_cstr).next() {
                if self.match_only_at_line_start && first_match.start != 0 {
                    return false;
                }
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub fn pattern_from_str(pat: &str) -> Pattern {
        Pattern::new(&ExpandedWord::unquoted_literal(pat)).expect("failed to create pattern")
    }

    pub fn filename_pattern_from_str(pat: &str) -> FilenamePattern {
        FilenamePattern::new(&ExpandedWord::unquoted_literal(pat))
            .expect("failed to create filename pattern")
    }

    fn cstring_from_str(s: &str) -> CString {
        CString::new(s).unwrap()
    }

    #[test]
    fn remove_largest_prefix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_largest_prefix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_smallest_prefix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_shortest_prefix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_largest_suffix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_largest_suffix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_smallest_suffix_from_empty_string() {
        assert_eq!(
            pattern_from_str("abcd").remove_shortest_suffix("".to_string()),
            ""
        )
    }

    #[test]
    fn remove_largest_prefix() {
        assert_eq!(
            pattern_from_str("*b").remove_largest_prefix("abaaaaabtest".to_string()),
            "test"
        )
    }

    #[test]
    fn remove_smallest_prefix() {
        assert_eq!(
            pattern_from_str("*b").remove_shortest_prefix("abaaaaabtest".to_string()),
            "aaaaabtest"
        )
    }

    #[test]
    fn remove_largest_suffix() {
        assert_eq!(
            pattern_from_str("b*").remove_largest_suffix("testbaaaaaba".to_string()),
            "test"
        )
    }

    #[test]
    fn remove_smallest_suffix() {
        assert_eq!(
            pattern_from_str("b*").remove_shortest_suffix("testbaaaaaba".to_string()),
            "testbaaaaa"
        )
    }

    #[test]
    fn filename_pattern_matches_simple_components_in_path() {
        let pattern = filename_pattern_from_str("/path/to/file");
        assert!(pattern.matches_all(1, &cstring_from_str("path")));
        assert!(pattern.matches_all(2, &cstring_from_str("to")));
        assert!(pattern.matches_all(3, &cstring_from_str("file")));
    }

    #[test]
    fn period_at_the_start_is_only_matched_explicitly() {
        let pattern = filename_pattern_from_str("*test");
        assert!(!pattern.matches_all(1, &cstring_from_str(".test")));
        assert!(pattern.matches_all(1, &cstring_from_str("atest")));

        let pattern = filename_pattern_from_str("/dir/*file");
        assert!(!pattern.matches_all(2, &cstring_from_str(".file")));

        let pattern = filename_pattern_from_str(".test");
        assert!(pattern.matches_all(1, &cstring_from_str(".test")));
    }

    #[test]
    fn period_at_the_start_is_not_matched_by_bracket_expression_with_multiple_chars() {
        // the standard leaves this case to the implementation, here we follow what bash does
        let pattern = filename_pattern_from_str("[.abc]*");
        assert!(!pattern.matches_all(1, &cstring_from_str(".a")));
    }

    #[test]
    fn match_history_pattern() {
        let pattern = HistoryPattern::new("arg".to_string()).unwrap().unwrap();
        assert!(pattern.matches("cmd arg"));

        let pattern = HistoryPattern::new("^cmd".to_string()).unwrap().unwrap();
        assert!(pattern.matches("cmd arg"));

        let pattern = HistoryPattern::new("^arg".to_string()).unwrap().unwrap();
        assert!(!pattern.matches("cmd arg"));
    }
}
