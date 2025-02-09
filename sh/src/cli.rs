use crate::interpreter::set::SetOptions;
use clap::command;

#[derive(Debug, PartialEq, Eq)]
pub enum ExecutionMode {
    Interactive,
    ReadCommandsFromStdin,
    ReadCommandsFromString(String),
    ReadFromFile(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ShellArgs {
    pub program_name: String,
    pub execution_mode: ExecutionMode,
    pub arguments: Vec<String>,
    pub set_options: SetOptions,
}

impl Default for ShellArgs {
    fn default() -> Self {
        ShellArgs {
            program_name: String::default(),
            execution_mode: ExecutionMode::Interactive,
            arguments: Vec::new(),
            set_options: SetOptions::default(),
        }
    }
}

pub fn parse_args(args: Vec<String>, is_attached_to_terminal: bool) -> Result<ShellArgs, String> {
    assert!(args.len() > 0, "missing program name");
    let mut execution_option = None;
    let mut set_options = SetOptions::default();

    let mut iter = args.into_iter();
    let mut program_name = iter.next().unwrap();
    let mut arguments = Vec::new();

    while let Some(next) = iter.next() {
        match next.as_str() {
            "-o" | "+o" => {
                let option = iter
                    .next()
                    .ok_or_else(|| format!("{} requires an option name", next))?;
                set_options.set_long(&option, next == "-o")?;
            }
            s if s.starts_with('-') => {
                for c in s.chars().skip(1) {
                    match c {
                        'c' | 's' | 'i' => {
                            if let Some(prev) = execution_option {
                                return Err(format!("{} and {} cannot be used together", prev, c));
                            } else {
                                execution_option = Some(c);
                            }
                        }
                        other => set_options.set_short(other, true)?,
                    }
                }
            }
            s if s.starts_with('+') => {
                for c in s.chars().skip(1) {
                    set_options.set_short(c, false)?;
                }
            }
            "-" => break,
            _ => {
                arguments.push(next);
                break;
            }
        }
    }
    arguments.extend(iter);

    let execution_mode = match execution_option {
        None => {
            if arguments.len() == 0 {
                if is_attached_to_terminal {
                    ExecutionMode::Interactive
                } else {
                    ExecutionMode::ReadCommandsFromStdin
                }
            } else {
                let command_file = arguments.drain(0..1).next().unwrap();
                ExecutionMode::ReadFromFile(command_file)
            }
        }
        Some('i') => ExecutionMode::Interactive,
        Some('c') => {
            if arguments.is_empty() {
                return Err("missing command string for -c".to_string());
            }
            if arguments.len() > 1 {
                let mut iter = arguments.drain(0..2);
                let command_string = iter.next().unwrap();
                program_name = iter.next().unwrap();
                ExecutionMode::ReadCommandsFromString(command_string)
            } else {
                let command_string = arguments.drain(0..1).next().unwrap();
                ExecutionMode::ReadCommandsFromString(command_string)
            }
        }
        Some('s') => ExecutionMode::ReadCommandsFromStdin,
        _ => unreachable!(),
    };

    Ok(ShellArgs {
        set_options,
        program_name,
        execution_mode,
        arguments,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_args(args: Vec<&str>, is_interactive: bool) -> ShellArgs {
        let args = args.iter().map(|s| s.to_string()).collect();
        super::parse_args(args, is_interactive).expect("could not parse args")
    }

    #[test]
    fn read_commands_from_string_no_args_no_options() {
        let parsed_args = parse_args(vec!["sh", "-c", "ls -la"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromString("ls -la".to_string())
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.set_options, SetOptions::default());
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(parsed_args, parse_args(vec!["sh", "-c", "ls -la"], true));
    }

    #[test]
    fn read_commands_from_string_with_args_no_options() {
        let parsed_args = parse_args(vec!["sh", "-c", "ls -la", "arg1", "arg2"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromString("ls -la".to_string())
        );
        assert_eq!(parsed_args.program_name, "arg1");
        assert_eq!(parsed_args.arguments, vec!["arg2"]);
        assert_eq!(parsed_args.set_options, SetOptions::default());
        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "-c", "ls -la", "arg1", "arg2"], true)
        );
    }

    #[test]
    fn read_commands_from_string_no_args_with_options() {
        let parsed_args = parse_args(
            vec!["sh", "-am", "+C", "-o", "errexit", "-c", "ls -la"],
            false,
        );
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromString("ls -la".to_string())
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                errexit: true,
                noclobber: false,
                allexport: true,
                monitor: true,
                ..SetOptions::default()
            }
        );
        assert_eq!(
            parsed_args,
            parse_args(
                vec!["sh", "-am", "+C", "-o", "errexit", "-c", "ls -la"],
                true
            )
        );
    }

    #[test]
    fn read_commands_from_string_with_args_with_options() {
        let parsed_args = parse_args(
            vec![
                "sh",
                "-cfu",
                "+x",
                "+o",
                "noclobber",
                "ls -la",
                "arg1",
                "arg2",
            ],
            false,
        );
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromString("ls -la".to_string())
        );
        assert_eq!(parsed_args.program_name, "arg1");
        assert_eq!(parsed_args.arguments, vec!["arg2"]);
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                noglob: true,
                nounset: true,
                xtrace: false,
                noclobber: false,
                ..SetOptions::default()
            }
        );
        assert_eq!(
            parsed_args,
            parse_args(
                vec![
                    "sh",
                    "-cfu",
                    "+x",
                    "+o",
                    "noclobber",
                    "ls -la",
                    "arg1",
                    "arg2"
                ],
                true
            )
        );
    }

    #[test]
    fn read_commands_from_stdin_no_args_no_options() {
        let parsed_args = parse_args(vec!["sh", "-s"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin,
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(parsed_args.set_options, SetOptions::default());

        assert_eq!(parsed_args, parse_args(vec!["sh", "-s"], true));
    }

    #[test]
    fn read_commands_from_stdin_with_args_no_options() {
        let parsed_args = parse_args(vec!["sh", "-s", "arg1", "arg2"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.arguments, vec!["arg1", "arg2"]);
        assert_eq!(parsed_args.set_options, SetOptions::default());

        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "-s", "arg1", "arg2"], true)
        );
    }

    #[test]
    fn read_commands_from_stdin_no_args_with_options() {
        let parsed_args = parse_args(vec!["sh", "-sbv", "+o", "vi", "+f"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                notify: true,
                verbose: true,
                vi: false,
                noglob: false,
                ..SetOptions::default()
            }
        );
        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "-sbv", "+o", "vi", "+f"], true)
        );
    }

    #[test]
    fn read_commands_from_stdin_with_args_with_options() {
        let parsed_args = parse_args(vec!["sh", "-sbv", "+o", "vi", "+f", "arg1", "arg2"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.arguments, vec!["arg1", "arg2"]);
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                notify: true,
                verbose: true,
                vi: false,
                noglob: false,
                ..SetOptions::default()
            }
        );
        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "-sbv", "+o", "vi", "+f", "arg1", "arg2"], true)
        );
    }

    #[test]
    fn default_execution_no_args_no_options_and_attached_to_terminal() {
        let parsed_args = parse_args(vec!["sh"], true);
        assert_eq!(parsed_args.execution_mode, ExecutionMode::Interactive);
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(parsed_args.set_options, SetOptions::default());
    }

    #[test]
    fn default_execution_with_args_no_options_and_attached_to_terminal() {
        let parsed_args = parse_args(vec!["sh", "file.sh", "arg2"], true);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadFromFile("file.sh".to_string())
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.arguments, vec!["arg2"]);
        assert_eq!(parsed_args.set_options, SetOptions::default());
        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "file.sh", "arg2"], false)
        );
    }

    #[test]
    fn default_execution_no_args_with_options_and_attached_to_terminal() {
        let parsed_args = parse_args(vec!["sh", "+eb"], true);
        assert_eq!(parsed_args.execution_mode, ExecutionMode::Interactive);
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                errexit: false,
                notify: false,
                ..Default::default()
            }
        );
    }

    #[test]
    fn default_execution_with_args_with_options_and_attached_to_terminal() {
        let parsed_args = parse_args(vec!["sh", "+o", "allexport", "file.sh", "arg2"], true);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadFromFile("file.sh".to_string())
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.arguments, vec!["arg2"]);
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                allexport: false,
                ..Default::default()
            }
        );
    }

    #[test]
    fn default_execution_no_args_no_options_and_not_attached_to_terminal() {
        let parsed_args = parse_args(vec!["sh"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin
        );
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(parsed_args.set_options, SetOptions::default());
    }

    #[test]
    fn interactive_no_options_no_args() {
        let parsed_args = parse_args(vec!["sh", "-i"], true);
        assert_eq!(parsed_args.execution_mode, ExecutionMode::Interactive);
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(parsed_args.set_options, SetOptions::default());
    }

    #[test]
    fn interactive_no_options_with_args() {
        let parsed_args = parse_args(vec!["sh", "-i", "arg1", "arg2"], true);
        assert_eq!(parsed_args.execution_mode, ExecutionMode::Interactive);
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.arguments, vec!["arg1", "arg2"]);
        assert_eq!(parsed_args.set_options, SetOptions::default());
    }

    #[test]
    fn interactive_with_options_no_args() {
        let parsed_args = parse_args(vec!["sh", "-i", "+o", "xtrace", "-o", "ignoreeof"], true);
        assert_eq!(parsed_args.execution_mode, ExecutionMode::Interactive);
        assert_eq!(parsed_args.program_name, "sh");
        assert!(parsed_args.arguments.is_empty());
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                xtrace: false,
                ignoreeof: true,
                ..SetOptions::default()
            }
        );
    }

    #[test]
    fn interactive_with_options_with_args() {
        let parsed_args = parse_args(vec!["sh", "-ina", "arg1", "arg2"], true);
        assert_eq!(parsed_args.execution_mode, ExecutionMode::Interactive);
        assert_eq!(parsed_args.program_name, "sh");
        assert_eq!(parsed_args.arguments, vec!["arg1", "arg2"]);
        assert_eq!(
            parsed_args.set_options,
            SetOptions {
                noexec: true,
                allexport: true,
                ..SetOptions::default()
            }
        );
    }
}
