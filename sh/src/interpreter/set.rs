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

pub enum ParsedArgs {
    WriteSettingsHumanReadable,
    WriteSettingsShellReadable,
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
                "-o" => return Ok(ParsedArgs::WriteSettingsHumanReadable),
                "+o" => return Ok(ParsedArgs::WriteSettingsShellReadable),
                "--" => return Ok(ParsedArgs::ResetPositionalParameters),
                _ => {}
            }
        }
        let mut options = SetOptions::default();
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--" if i == 0 => break,
                "-o" | "+o" => {
                    i += 1;
                    let option = args
                        .get(i)
                        .ok_or_else(|| "expected option".to_string())?
                        .as_str();
                    options.set_long(option, args[i - 1] == "-o")?
                }
                s if s.starts_with('-') || s.starts_with('+') => {
                    let option_value = s.starts_with('-');
                    for c in s.chars().skip(1) {
                        options.set_short(c, option_value)?
                    }
                }
                _ => break,
            }
        }
        Ok(ParsedArgs::ArgsStart(i))
    }
}
