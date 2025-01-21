use std::ffi::{CStr, CString};
use crate::interpreter::ExpandedWord;
use crate::interpreter::pattern::parse::parse_pattern;
use crate::interpreter::pattern::regex::{parsed_pattern_to_regex, Regex};

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

    pub fn remove_largest_prefix(&self, s: String) -> String {
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
        assert!(!s.as_bytes().contains(&b'\0'), "trying to match a string containing null");
        let mut bytes = s.into_bytes();
        bytes.push(b'\0');
        let mut prefix_end = 0;
        for i in 1..bytes.len() - 1 {
            let t = bytes[i];
            bytes[i] = b'\0';
            // we know there is a null, so this unwrap will never fail
            if self.regex.matches(CStr::from_bytes_until_nul(&bytes).unwrap()) {
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
        assert!(!s.as_bytes().contains(&b'\0'), "trying to match a string containing null");
        let mut bytes = s.into_bytes();
        bytes.push(b'\0');
        let mut suffix_start = bytes.len();
        for i in (1..bytes.len() - 2).rev() {
            // we know there is a null, so this unwrap will never fail
            if self.regex.matches(CStr::from_bytes_until_nul(&bytes[i..]).unwrap()) {
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

#[cfg(test)]
mod tests {
    use crate::interpreter::ExpandedWordPart;
    use super::*;

    fn pattern(pat: &str) -> Pattern {
        Pattern::new(&ExpandedWord { parts: vec![ExpandedWordPart::UnquotedLiteral(pat.to_string())] }).expect("failed to create pattern")
    }

    #[test]
    fn remove_largest_prefix() {
        assert_eq!(pattern("*b").remove_largest_prefix("abaaaaabtest".to_string()), "test")
    }

    #[test]
    fn remove_smallest_prefix() {
        assert_eq!(pattern("*b").remove_shortest_prefix("abaaaaabtest".to_string()), "aaaaabtest")
    }

    #[test]
    fn remove_largest_suffix() {
        assert_eq!(pattern("b*").remove_largest_suffix("testbaaaaaba".to_string()), "test")
    }

    #[test]
    fn remove_smallest_suffix() {
        assert_eq!(pattern("b*").remove_shortest_suffix("testbaaaaaba".to_string()), "testbaaaaa")
    }
}