//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;
use std::ffi::CString;
use std::fmt::Formatter;
use std::ptr;

fn regex_compilation_result(
    status_integer: libc::c_int,
    regex: &libc::regex_t,
) -> Result<(), String> {
    if status_integer != 0 {
        let mut error_buffer = vec![b'\0'; 128];
        unsafe {
            libc::regerror(
                status_integer,
                ptr::from_ref(regex),
                error_buffer.as_mut_ptr() as *mut libc::c_char,
                128,
            )
        };
        let error = CString::from_vec_with_nul(error_buffer)
            .expect("error message returned from `libc::regerror` is an invalid CString");
        Err(error
            .into_string()
            .expect("error message from `libc::regerror' contains invalid utf-8"))
    } else {
        Ok(())
    }
}

pub struct Regex {
    raw_regex: libc::regex_t,
    regex_string: CString,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct RegexMatch {
    pub start: usize,
    pub end: usize,
}

pub struct MatchIter<'re> {
    string: CString,
    next_start: usize,
    regex: &'re Regex,
}

impl Iterator for MatchIter<'_> {
    type Item = RegexMatch;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_start >= self.string.as_bytes().len() {
            return None;
        }
        let mut match_range = libc::regmatch_t {
            rm_so: -1,
            rm_eo: -1,
        };
        let exec_status = unsafe {
            libc::regexec(
                ptr::from_ref(&self.regex.raw_regex),
                self.string.as_ptr().add(self.next_start),
                1,
                ptr::from_mut(&mut match_range),
                0,
            )
        };
        if exec_status == libc::REG_NOMATCH {
            return None;
        }
        let result = RegexMatch {
            start: self.next_start + match_range.rm_so as usize,
            end: self.next_start + match_range.rm_eo as usize,
        };
        self.next_start += match_range.rm_eo as usize;
        Some(result)
    }
}

impl Regex {
    pub fn new(regex: CString) -> Result<Self, String> {
        let mut raw = unsafe { std::mem::zeroed::<libc::regex_t>() };
        let compilation_status =
            unsafe { libc::regcomp(ptr::from_mut(&mut raw), regex.as_ptr(), libc::REG_EXTENDED) };
        regex_compilation_result(compilation_status, &raw)?;
        Ok(Self {
            raw_regex: raw,
            regex_string: regex,
        })
    }

    pub fn match_locations(&self, string: CString) -> MatchIter {
        MatchIter {
            next_start: 0,
            regex: self,
            string,
        }
    }

    pub fn matches(&self, string: &CString) -> bool {
        let exec_status = unsafe {
            libc::regexec(
                ptr::from_ref(&self.raw_regex),
                string.as_ptr(),
                0,
                ptr::null_mut(),
                0,
            )
        };
        exec_status != libc::REG_NOMATCH
    }

    pub fn str(&self) -> &str {
        self.regex_string
            .to_str()
            .expect("regex string contains invalid utf8")
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe {
            libc::regfree(ptr::from_mut(&mut self.raw_regex));
        }
    }
}

impl fmt::Debug for Regex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "/{}/", self.regex_string.to_str().unwrap())
    }
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.regex_string == other.regex_string
    }
}

/// utility function for writing tests
#[cfg(test)]
pub fn regex_from_str(re: &str) -> Regex {
    Regex::new(CString::new(re).unwrap()).expect("error compiling ere")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_create_regex() {
        regex_from_str("test");
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_regex_matches() {
        let ere = regex_from_str("ab*c");
        assert!(ere.matches(&CString::new("abbbbc").unwrap()));
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_regex_match_locations() {
        let ere = regex_from_str("match");
        let mut iter = ere.match_locations(CString::new("match 12345 match2 matchmatch").unwrap());
        assert_eq!(iter.next(), Some(RegexMatch { start: 0, end: 5 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 12, end: 17 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 19, end: 24 }));
        assert_eq!(iter.next(), Some(RegexMatch { start: 24, end: 29 }));
        assert_eq!(iter.next(), None);
    }
}
