#[derive(Default, Debug, PartialEq, Eq)]
pub struct SetOptions {
    /// -a
    pub allexport: bool,
    /// -b
    pub notify: bool,
    /// -C
    pub noclobber: bool,
    /// -e
    pub errexit: bool,
    /// -f
    pub noglob: bool,
    /// -h
    pub cache_location_of_utilities_in_functions: bool,
    /// -m
    pub monitor: bool,
    /// -n
    pub noexec: bool,
    /// -u
    pub nounset: bool,
    /// -v
    pub verbose: bool,
    /// -x
    pub xtrace: bool,
    pub ignoreeof: bool,
    pub nolog: bool,
    pub vi: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ParsedArgs {
    PrintSettingsHumanReadable,
    PrintSettingsShellReadable,
    ResetPositionalParameters,
    PrintVars,
    ArgsStart(usize),
}

impl SetOptions {
    pub fn set_long(&mut self, long_option: &str, value: bool) -> Result<(), String> {
        match long_option {
            "allexport" => self.allexport = value,
            "errexit" => self.errexit = value,
            "ignoreeof" => self.ignoreeof = value,
            "monitor" => self.monitor = value,
            "noclobber" => self.noclobber = value,
            "noglob" => self.noglob = value,
            "noexec" => self.noexec = value,
            "nolog" => self.nolog = value,
            "notify" => self.notify = value,
            "nounset" => self.nounset = value,
            "vi" => self.vi = value,
            "xtrace" => self.xtrace = value,
            _ => return Err(format!("invalid option '{}'", long_option)),
        }
        Ok(())
    }

    pub fn set_short(&mut self, short_option: char, value: bool) -> Result<(), String> {
        match short_option {
            'a' => self.allexport = value,
            'b' => self.notify = value,
            'C' => self.noclobber = value,
            'e' => self.errexit = value,
            'f' => self.noglob = value,
            'h' => self.cache_location_of_utilities_in_functions = value,
            'm' => self.monitor = value,
            'n' => self.noexec = value,
            'u' => self.nounset = value,
            'v' => self.verbose = value,
            'x' => self.xtrace = value,
            _ => return Err(format!("invalid option '{}'", short_option)),
        }
        Ok(())
    }

    pub fn to_string_human_readable(&self) -> String {
        todo!()
    }

    pub fn to_string_shell_readable(&self) -> String {
        todo!()
    }

    pub fn parse_args_and_update(&mut self, args: &[String]) -> Result<ParsedArgs, String> {
        if args.len() == 0 {
            return Ok(ParsedArgs::PrintVars);
        }
        if args.len() == 1 {
            match args[0].as_str() {
                "-o" => return Ok(ParsedArgs::PrintSettingsHumanReadable),
                "+o" => return Ok(ParsedArgs::PrintSettingsShellReadable),
                "--" => return Ok(ParsedArgs::ResetPositionalParameters),
                _ => {}
            }
        }
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--" if i == 0 => {
                    i += 1;
                    break;
                }
                "-o" | "+o" => {
                    i += 1;
                    let option = args
                        .get(i)
                        .ok_or_else(|| "expected option".to_string())?
                        .as_str();
                    self.set_long(option, args[i - 1] == "-o")?
                }
                s if s.starts_with('-') || s.starts_with('+') => {
                    let option_value = s.starts_with('-');
                    for c in s.chars().skip(1) {
                        self.set_short(c, option_value)?
                    }
                }
                _ => break,
            }
            i += 1;
        }
        Ok(ParsedArgs::ArgsStart(i))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_args(args: Vec<&str>) -> (SetOptions, ParsedArgs) {
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let mut options = SetOptions::default();
        let parsed_args = options
            .parse_args_and_update(&args)
            .expect("failed to parse args");
        (options, parsed_args)
    }

    #[test]
    fn print_human_readable() {
        let (options, parse_result) = parse_args(vec!["-o"]);
        assert_eq!(parse_result, ParsedArgs::PrintSettingsHumanReadable);
        assert_eq!(options, SetOptions::default());
    }

    #[test]
    fn print_shell_readable() {
        let (options, parse_result) = parse_args(vec!["+o"]);
        assert_eq!(parse_result, ParsedArgs::PrintSettingsShellReadable);
        assert_eq!(options, SetOptions::default());
    }

    #[test]
    fn no_args_prints_environment_variables() {
        let (options, parse_result) = parse_args(vec![]);
        assert_eq!(parse_result, ParsedArgs::PrintVars);
        assert_eq!(options, SetOptions::default());
    }

    #[test]
    fn reset_positional_parameters() {
        let (options, parse_result) = parse_args(vec!["--"]);
        assert_eq!(parse_result, ParsedArgs::ResetPositionalParameters);
        assert_eq!(options, SetOptions::default());
    }

    #[test]
    fn parse_positive_short_options_no_args() {
        let (options, parse_result) = parse_args(vec!["-a", "-nmh", "-C"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(3));
        assert_eq!(
            options,
            SetOptions {
                allexport: true,
                noexec: true,
                monitor: true,
                cache_location_of_utilities_in_functions: true,
                noclobber: true,
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_negative_short_options_no_args() {
        let (options, parse_result) = parse_args(vec!["+xvu", "+e", "+x"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(3));
        assert_eq!(
            options,
            SetOptions {
                xtrace: false,
                verbose: false,
                nounset: false,
                errexit: false,
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_short_options_with_args() {
        let (options, parse_result) = parse_args(vec!["-hm", "+ab", "arg1", "arg2"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(2));
        assert_eq!(
            options,
            SetOptions {
                cache_location_of_utilities_in_functions: true,
                monitor: true,
                allexport: false,
                notify: false,
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_args_starting_with_minus() {
        let (options, parse_result) = parse_args(vec!["--", "-arg", "arg2"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(1));
        assert_eq!(options, SetOptions::default());
    }

    #[test]
    fn parse_args_starting_with_plus() {
        let (options, parse_result) = parse_args(vec!["--", "+arg", "arg2"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(1));
        assert_eq!(options, SetOptions::default());
    }

    #[test]
    fn parse_positive_long_options_no_args() {
        let (options, parse_result) = parse_args(vec!["-o", "ignoreeof", "-o", "notify"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(4));
        assert_eq!(
            options,
            SetOptions {
                ignoreeof: true,
                notify: true,
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_negative_long_options_no_args() {
        let (options, parse_result) = parse_args(vec!["+o", "vi", "+o", "nolog"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(4));
        assert_eq!(
            options,
            SetOptions {
                vi: false,
                nolog: false,
                ..Default::default()
            }
        );
    }

    #[test]
    fn parse_long_options_with_args() {
        let (options, parse_result) =
            parse_args(vec!["-o", "nounset", "+o", "noglob", "arg1", "arg2"]);
        assert_eq!(parse_result, ParsedArgs::ArgsStart(4));
        assert_eq!(
            options,
            SetOptions {
                nounset: true,
                noglob: false,
                ..Default::default()
            }
        );
    }
}
