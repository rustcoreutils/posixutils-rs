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

    pub fn next_argument(&self) -> usize {
        self.arg_pos
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_args(args: Vec<&str>) -> Vec<String> {
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
}
