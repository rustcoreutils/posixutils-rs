//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::shstr::ShString;
use gettextrs::gettext;

struct OptsParser<'s> {
    optstring: &'s str,
}

#[derive(Debug, PartialEq, Eq)]
enum ParseResult {
    SimpleOption(char),
    OptionWithArg { option: char, arg: ShString },
    InvalidOption(char),
    MissingArg(char),
    EndOfOptions,
}

impl<'s> OptsParser<'s> {
    fn new(optstring: &'s str) -> Result<Self, String> {
        if !optstring
            .chars()
            .all(|c| c.is_ascii_alphabetic() || c == ':')
        {
            return Err(gettext("getopts: invalid option string"));
        }
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

    fn parse(
        &self,
        params: &[ShString],
        param_index: &mut usize,
        option_index: &mut usize,
    ) -> ParseResult {
        let mut i = 0;
        while *param_index < params.len() {
            let param = &params[*param_index];
            match param.as_bytes() {
                b"--" => {
                    *param_index += 1;
                    *option_index = 0;
                    return ParseResult::EndOfOptions;
                }
                options if options.starts_with(b"-") && options.len() > 1 => {
                    // Scanned over the raw bytes: an option letter is ASCII, so
                    // one byte each, and a byte that is not one simply is not an
                    // option. Scanning a lossy view instead made every invalid
                    // byte three bytes wide there but one here, so the offset of
                    // an inline option-argument indexed past the end.
                    for (pos, byte) in options[1..].iter().enumerate() {
                        let c = char::from(*byte);
                        if !byte.is_ascii() {
                            return ParseResult::InvalidOption(c);
                        }
                        if let Some(requires_argument) = self.get_option(c) {
                            if requires_argument {
                                if pos + 2 < options.len() {
                                    if i == *option_index {
                                        *param_index += 1;
                                        *option_index = 0;
                                        return ParseResult::OptionWithArg {
                                            option: c,
                                            arg: ShString::from(options[pos + 2..].to_vec()),
                                        };
                                    }
                                    break;
                                } else {
                                    *option_index = 0;
                                    if let Some(arg) = params.get(*param_index + 1) {
                                        *param_index += 2;
                                        return ParseResult::OptionWithArg {
                                            option: c,
                                            arg: arg.to_sh_string(),
                                        };
                                    }
                                    *param_index += 1;
                                    return ParseResult::MissingArg(c);
                                }
                            } else if i == *option_index {
                                advance_position(
                                    param_index,
                                    option_index,
                                    pos + 1 >= options.len() - 1,
                                );
                                return ParseResult::SimpleOption(c);
                            }
                        } else if i == *option_index {
                            advance_position(
                                param_index,
                                option_index,
                                pos + 1 >= options.len() - 1,
                            );
                            return ParseResult::InvalidOption(c);
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

/// After returning an option, advance to the next within-argument position; if
/// the option was the argument's last character, advance to the next argument
/// so OPTIND points past it (matching POSIX/bash OPTIND semantics).
fn advance_position(param_index: &mut usize, option_index: &mut usize, is_last_in_arg: bool) {
    if is_last_in_arg {
        *param_index += 1;
        *option_index = 0;
    } else {
        *option_index += 1;
    }
}

pub struct GetOpts;

impl BuiltinUtility for GetOpts {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = skip_option_terminator(args);
        if args.len() < 2 {
            return Err(gettext("getopts: missing arguments").into());
        }

        // The option string is a list of option letters: text by definition.
        let optstring = args[0].to_str().ok_or_else(|| {
            format!(
                "getopts: '{}' is not a valid option string",
                args[0].display()
            )
        })?;
        let quiet_errs = optstring.starts_with(':');
        let parser = OptsParser::new(&optstring[quiet_errs as usize..])?;
        // OPTIND is a plain 1-based integer (the next argument to inspect). The
        // within-argument position for bundled options is tracked in shell state
        // and reset whenever the application changes OPTIND.
        let optind_val: usize = shell
            .environment
            .get_str_value("OPTIND")
            .and_then(|v| v.parse().ok())
            .filter(|&n| n >= 1)
            .unwrap_or(1);
        let mut parameter_index = optind_val - 1;
        let mut option_index = if optind_val == shell.getopts_state.0 {
            shell.getopts_state.1
        } else {
            0
        };

        let var_name = args[1].to_str().unwrap_or("");
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
                shell.environment.unset("OPTARG")?;
                0
            }
            ParseResult::OptionWithArg { option, arg } => {
                shell.assign_global(var_name.to_string(), option.to_string())?;
                shell.assign_global("OPTARG".to_string(), arg)?;
                0
            }
            ParseResult::InvalidOption(opt) => {
                shell.assign_global(var_name.to_string(), "?".to_string())?;
                if quiet_errs {
                    shell.assign_global("OPTARG".to_string(), opt.to_string())?;
                } else {
                    shell.environment.unset("OPTARG")?;
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
                    shell.environment.unset("OPTARG")?;
                    opened_files.write_err(format!("getopts: option {opt} requires an argument\n"));
                }
                0
            }
            ParseResult::EndOfOptions => {
                // POSIX: at end of options the name variable is set to '?'.
                shell.assign_global(var_name.to_string(), "?".to_string())?;
                1
            }
        };

        let new_optind = parameter_index + 1;
        shell.assign_global("OPTIND".to_string(), new_optind.to_string())?;
        shell.getopts_state = (new_optind, option_index);

        Ok(status)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_options_in_separate_params() {
        let params = vec![
            ShString::from("-a"),
            ShString::from("-b"),
            ShString::from("-c"),
        ];
        let parser = OptsParser::new("abc").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('b')
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('c')
        );
        assert_eq!(param_index, 3);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_simple_options_in_same_param() {
        let params = vec![ShString::from("-abc")];
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
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_options_with_args_in_separate_params() {
        let params = vec![
            ShString::from("-a"),
            ShString::from("arg1"),
            ShString::from("-b"),
            ShString::from("arg2"),
            ShString::from("-c"),
            ShString::from("arg3"),
        ];
        let parser = OptsParser::new("a:b:c:").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'a',
                arg: ShString::from("arg1")
            }
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'b',
                arg: ShString::from("arg2")
            }
        );
        assert_eq!(param_index, 4);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'c',
                arg: ShString::from("arg3")
            }
        );
        assert_eq!(param_index, 6);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_options_with_args_in_same_param() {
        let params = vec![
            ShString::from("-aarg1"),
            ShString::from("-barg2"),
            ShString::from("-carg3"),
        ];
        let parser = OptsParser::new("a:b:c:").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'a',
                arg: ShString::from("arg1")
            }
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'b',
                arg: ShString::from("arg2")
            }
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'c',
                arg: ShString::from("arg3")
            }
        );
        assert_eq!(param_index, 3);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_mixed_options() {
        let params = vec![
            ShString::from("-abc"),
            ShString::from("arg1"),
            ShString::from("-df"),
            ShString::from("arg2"),
            ShString::from("-larg3"),
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
                arg: ShString::from("arg1")
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
                arg: ShString::from("arg2")
            }
        );
        assert_eq!(param_index, 4);
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::OptionWithArg {
                option: 'l',
                arg: ShString::from("arg3")
            }
        );
        assert_eq!(param_index, 5);
        assert_eq!(options_index, 0);
    }

    #[test]
    fn parse_options_followed_by_operands() {
        let params = vec![
            ShString::from("-ab"),
            ShString::from("-c"),
            ShString::from("op1"),
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
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('c')
        );
        assert_eq!(param_index, 2);
        assert_eq!(options_index, 0);
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
            ShString::from("-a"),
            ShString::from("-bc"),
            ShString::from("--"),
            ShString::from("op1"),
        ];
        let parser = OptsParser::new("abc").unwrap();
        let mut param_index = 0;
        let mut options_index = 0;
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::SimpleOption('a')
        );
        assert_eq!(param_index, 1);
        assert_eq!(options_index, 0);
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
        assert_eq!(options_index, 0);
        assert_eq!(
            parser.parse(&params, &mut param_index, &mut options_index),
            ParseResult::EndOfOptions
        );
        assert_eq!(param_index, 3);
        assert_eq!(options_index, 0);
    }
}
