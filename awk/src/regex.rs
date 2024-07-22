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
}

#[derive(Copy, Clone, Debug, Default)]
pub struct RegexMatch {
    pub start: usize,
    pub end: usize,
}

impl Regex {
    pub fn new(regex: CString) -> Result<Self, String> {
        let mut raw = unsafe { std::mem::zeroed::<libc::regex_t>() };
        let compilation_status =
            unsafe { libc::regcomp(ptr::from_mut(&mut raw), regex.as_ptr(), libc::REG_EXTENDED) };
        regex_compilation_result(compilation_status, &raw)?;
        Ok(Self { raw_regex: raw })
    }

    pub fn match_locations(
        &self,
        string: CString,
        match_buffer: &mut Vec<RegexMatch>,
        max_count: usize,
    ) {
        match_buffer.clear();
        let mut next_start = 0;
        for _ in 0..max_count {
            let mut match_range = libc::regmatch_t {
                rm_so: -1,
                rm_eo: -1,
            };
            let exec_status = unsafe {
                libc::regexec(
                    ptr::from_ref(&self.raw_regex),
                    string.as_ptr().add(next_start),
                    1,
                    ptr::from_mut(&mut match_range),
                    0,
                )
            };
            if exec_status == libc::REG_NOMATCH {
                break;
            }
            match_buffer.push(RegexMatch {
                start: next_start + match_range.rm_so as usize,
                end: next_start + match_range.rm_eo as usize,
            });
            next_start += match_range.rm_eo as usize;
        }
    }

    pub fn matches(&self, string: CString) -> bool {
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
        writeln!(f, "<compiled regex>")
    }
}

impl PartialEq for Regex {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_regex() {
        Regex::new(CString::new("a").unwrap()).expect("error compiling ere");
    }

    #[test]
    fn test_regex_matches() {
        let ere = Regex::new(CString::new("ab*c").unwrap()).expect("error compiling ere");
        assert!(ere.matches(CString::new("abbbbc").unwrap()));
    }

    #[test]
    fn test_regex_match_locations() {
        let ere = Regex::new(CString::new("match").unwrap()).expect("error compiling ere");
        let mut match_buffer = Vec::new();
        ere.match_locations(
            CString::new("match 12345 match2 matchmatch").unwrap(),
            &mut match_buffer,
            4,
        );
        assert_eq!(match_buffer[0].start, 0);
        assert_eq!(match_buffer[0].end, 5);
        assert_eq!(match_buffer[1].start, 12);
        assert_eq!(match_buffer[1].end, 17);
        assert_eq!(match_buffer[2].start, 19);
        assert_eq!(match_buffer[2].end, 24);
        assert_eq!(match_buffer[3].start, 24);
        assert_eq!(match_buffer[3].end, 29);
    }
}
