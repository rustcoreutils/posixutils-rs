use crate::builtin::{skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

struct OptsParser<'s> {
    optstring: &'s str,
}

#[derive(Debug, PartialEq, Eq)]
enum ParseResult<'s> {
    SimpleOption(char),
    OptionWithArg { option: char, arg: &'s str },
    InvalidOption(char),
    MissingArg(char),
    EndOfOptions,
}

impl<'s> OptsParser<'s> {
    fn new(optstring: &'s str) -> Result<Self, String> {
        // TODO: check that optstring is valid
        Ok(Self { optstring })
    }

    fn get_option(&self, option_char: char) -> Option<bool> {
        let pos = self.optstring.find(option_char)?;
        if self.optstring[pos + option_char.len_utf8()..].starts_with(':') {
            Some(true)
        } else {
            Some(false)
        }
    }

    fn parse<'a>(
        &self,
        params: &'a [String],
        param_index: &mut usize,
        option_index: &mut usize,
    ) -> ParseResult<'a> {
        let mut i = 0;
        while *param_index < params.len() {
            let param = &params[*param_index];
            match param.as_str() {
                "--" => {
                    *param_index += 1;
                    *option_index = 0;
                    return ParseResult::EndOfOptions;
                }
                options if options.starts_with("-") && options.len() > 1 => {
                    for (pos, c) in options[1..].char_indices() {
                        if let Some(requires_argument) = self.get_option(c) {
                            if requires_argument {
                                if pos + c.len_utf8() + 1 < options.len() {
                                    if i == *option_index {
                                        *param_index += 1;
                                        *option_index = 0;
                                        return ParseResult::OptionWithArg {
                                            option: c,
                                            arg: &options[1 + pos + c.len_utf8()..],
                                        };
                                    }
                                    i += 1;
                                    break;
                                } else {
                                    *option_index = 0;
                                    if let Some(arg) = params.get(*param_index + 1) {
                                        *param_index += 2;
                                        return ParseResult::OptionWithArg { option: c, arg };
                                    }
                                    *param_index += 1;
                                    return ParseResult::MissingArg(c);
                                }
                            } else if i == *option_index {
                                *option_index += 1;
                                return ParseResult::SimpleOption(c);
                            }
                        } else {
                            if i == *option_index {
                                *option_index += 1;
                                return ParseResult::InvalidOption(c);
                            }
                        }
                        i += 1;
                    }
                }
                _ => return ParseResult::EndOfOptions,
            }
            i = 0;
            *option_index = 0;
            *param_index += 1;
        }
        ParseResult::EndOfOptions
    }
}

fn parse_optind(value: &str) -> Result<(usize, usize), &'static str> {
    if let Some(pos) = value.find(':') {
        let (parameter, option) = value.split_at(pos);
        let parameter_index = parameter
            .parse::<usize>()
            .map_err(|_| "getopts: invalid OPTIND")?;
        if parameter_index < 1 {
            return Err("getopts: invalid OPTIND");
        }
        let option_index = option[1..]
            .parse::<usize>()
            .map_err(|_| "getopts: invalid OPTIND")?;
        Ok((parameter_index - 1, option_index))
    } else {
        let parameter_index = value
            .parse::<usize>()
            .map_err(|_| "getopts: invalid OPTIND")?;
        if parameter_index < 1 {
            return Err("getopts: invalid OPTIND");
        }
        Ok((parameter_index - 1, 0))
    }
}

fn optind_string(parameter_index: usize, option_index: usize) -> String {
    if option_index == 0 {
        parameter_index.to_string()
    } else {
        format!("{parameter_index}:{option_index}")
    }
}

pub struct GetOpts;

impl BuiltinUtility for GetOpts {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.len() < 2 {
            return Err("getopts: missing arguments".into());
        }

        let quiet_errs = args[0].starts_with(':');
        let parser = OptsParser::new(&args[0][quiet_errs as usize..])?;
        let optind = shell.variables.get_str_value("OPTIND").unwrap_or("1");
        let (mut parameter_index, mut option_index) = parse_optind(optind)?;

        let var_name = args[1].as_str();
        if !is_valid_name(var_name) {
            return Err(format!("getopts: '{var_name}' is not a valid variable name").into());
        }

        let parameters = if args.len() == 2 {
            &shell.positional_parameters
        } else {
            &args[2..]
        };

        let status = match parser.parse(parameters, &mut parameter_index, &mut option_index) {
            ParseResult::SimpleOption(opt) => {
                shell.assign_global(var_name.to_string(), opt.to_string())?;
                shell.variables.unset("OPTARG")?;
                0
            }
            ParseResult::OptionWithArg { option, arg } => {
                let arg = arg.to_string();
                shell.assign_global(var_name.to_string(), option.to_string())?;
                shell.assign_global("OPTARG".to_string(), arg)?;
                0
            }
            ParseResult::InvalidOption(opt) => {
                shell.assign_global(var_name.to_string(), "?".to_string())?;
                if quiet_errs {
                    shell.assign_global("OPTARG".to_string(), opt.to_string())?;
                } else {
                    shell.variables.unset("OPTARG")?;
                    opened_files.write_err(format!("getopts: illegal option {opt}\n"));
                }
                0
            }
            ParseResult::MissingArg(opt) => {
                if quiet_errs {
                    shell.assign_global(var_name.to_string(), ":".to_string())?;
                    shell.assign_global("OPTARG".to_string(), opt.to_string())?;
                } else {
                    shell.assign_global(var_name.to_string(), "?".to_string())?;
                    shell.variables.unset("OPTARG")?;
                    opened_files.write_err(format!("getopts: option {opt} requires an argument\n"));
                }
                0
            }
            ParseResult::EndOfOptions => 1,
        };

        shell.assign_global(
            "OPTIND".to_string(),
            optind_string(parameter_index + 1, option_index),
        )?;

        Ok(status)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_options_in_separate_params() {
        let params = vec!["-a".to_string(), "-b".to_string(), "-c".to_string()];
        let parser = OptsParser::new("abc").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('b')
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('c')
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 1);
    }

    #[test]
    fn parse_simple_options_in_same_param() {
        let params = vec!["-abc".to_string()];
        let parser = OptsParser::new("abc").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('b')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 2);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('c')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 3);
    }

    #[test]
    fn parse_options_with_args_in_separate_params() {
        let params = vec![
            "-a".to_string(),
            "arg1".to_string(),
            "-b".to_string(),
            "arg2".to_string(),
            "-c".to_string(),
            "arg3".to_string(),
        ];
        let parser = OptsParser::new("a:b:c:").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'a',
                arg: "arg1"
            }
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'b',
                arg: "arg2"
            }
        );
        assert_eq!(param_index, 4);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'c',
                arg: "arg3"
            }
        );
        assert_eq!(param_index, 6);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_options_with_args_in_same_param() {
        let params = vec![
            "-aarg1".to_string(),
            "-barg2".to_string(),
            "-carg3".to_string(),
        ];
        let parser = OptsParser::new("a:b:c:").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'a',
                arg: "arg1"
            }
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'b',
                arg: "arg2"
            }
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'c',
                arg: "arg3"
            }
        );
        assert_eq!(param_index, 3);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_mixed_options() {
        let params = vec![
            "-abc".to_string(),
            "arg1".to_string(),
            "-df".to_string(),
            "arg2".to_string(),
            "-larg3".to_string(),
        ];
        let parser = OptsParser::new("abc:df:l:").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('b')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 2);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'c',
                arg: "arg1"
            }
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('d')
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'f',
                arg: "arg2"
            }
        );
        assert_eq!(param_index, 4);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'l',
                arg: "arg3"
            }
        );
        assert_eq!(param_index, 5);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_options_followed_by_operands() {
        let params = vec!["-ab".to_string(), "-c".to_string(), "op1".to_string()];
        let parser = OptsParser::new("abc").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('b')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 2);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('c')
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::EndOfOptions
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_options_followed_by_operands_after_options_terminator() {
        let params = vec![
            "-a".to_string(),
            "-bc".to_string(),
            "--".to_string(),
            "op1".to_string(),
        ];
        let parser = OptsParser::new("abc").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 0);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('b')
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 1);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('c')
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 2);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::EndOfOptions
        );
        assert_eq!(param_index, 3);
        assert_eq!(options_index, 0);
    }
}
