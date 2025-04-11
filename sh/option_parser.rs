//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub struct OptionParser<'a> {
    args: &'a [String],
    arg_pos: usize,
    option_pos: usize,
}

impl<'a> OptionParser<'a> {
    pub fn new(args: &'a [String]) -> Self {
        Self {
            args,
            arg_pos: 0,
            option_pos: 0,
        }
    }

    pub fn next_option(&mut self) -> Result<Option<char>, &str> {
        while self.arg_pos < self.args.len() {
            let arg = &self.args[self.arg_pos];
            if self.option_pos == 0 {
                if arg == "--" {
                    self.arg_pos += 1;
                    return Ok(None);
                } else if !arg.starts_with('-') || arg.len() == 1 {
                    return Ok(None);
                }
                self.option_pos += 1;
            }
            if self.option_pos == arg.len() {
                self.arg_pos += 1;
                self.option_pos = 0;
                continue;
            }
            let c = arg[self.option_pos..].chars().next().unwrap();
            if !c.is_ascii_alphanumeric() {
                return Err(arg);
            }
            self.option_pos += 1;
            return Ok(Some(c));
        }
        Ok(None)
    }

    pub fn next_option_argument(&mut self) -> Option<&'a str> {
        if self.arg_pos < self.args.len() {
            let arg = &self.args[self.arg_pos];
            if self.option_pos == arg.len() {
                self.option_pos = 0;
                let result = self.args.get(self.arg_pos + 1).map(|s| s.as_str());
                self.arg_pos += 2;
                result
            } else {
                let result = Some(&self.args[self.arg_pos][self.option_pos..]);
                self.arg_pos += 1;
                self.option_pos = 0;
                result
            }
        } else {
            None
        }
    }

    pub fn next_argument(&self) -> usize {
        self.arg_pos
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub fn to_args(args: Vec<&str>) -> Vec<String> {
        args.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn parse_options_in_separate_arguments() {
        let args = to_args(vec!["-a", "-b", "-c"]);
        let mut parser = OptionParser::new(&args);
        assert_eq!(parser.next_option(), Ok(Some('a')));
        assert_eq!(parser.next_option(), Ok(Some('b')));
        assert_eq!(parser.next_option(), Ok(Some('c')));
        assert_eq!(parser.next_option(), Ok(None));
        assert_eq!(parser.next_argument(), 3);
    }

    #[test]
    fn parse_options_in_same_argument() {
        let args = to_args(vec!["-abc"]);
        let mut parser = OptionParser::new(&args);
        assert_eq!(parser.next_option(), Ok(Some('a')));
        assert_eq!(parser.next_option(), Ok(Some('b')));
        assert_eq!(parser.next_option(), Ok(Some('c')));
        assert_eq!(parser.next_option(), Ok(None));
        assert_eq!(parser.next_argument(), 1);
    }

    #[test]
    fn skip_options_terminator() {
        let args = to_args(vec!["-a", "--", "-b"]);
        let mut parser = OptionParser::new(&args);
        assert_eq!(parser.next_option(), Ok(Some('a')));
        assert_eq!(parser.next_option(), Ok(None));
        assert_eq!(parser.next_argument(), 2);
    }

    #[test]
    fn parse_option_argument() {
        let args = to_args(vec!["-a", "arg", "-barg2", "-c"]);
        let mut parser = OptionParser::new(&args);
        assert_eq!(parser.next_option(), Ok(Some('a')));
        assert_eq!(parser.next_option_argument(), Some("arg"));
        assert_eq!(parser.next_option(), Ok(Some('b')));
        assert_eq!(parser.next_option_argument(), Some("arg2"));
        assert_eq!(parser.next_option(), Ok(Some('c')));
        assert_eq!(parser.next_option_argument(), None);
    }
}
