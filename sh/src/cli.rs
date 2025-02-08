#[derive(Debug, PartialEq, Eq)]
pub enum ExecutionMode {
    Interactive,
    ReadCommandsFromStdin,
    ReadCommandsFromString(String),
    ReadFromFile(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ShellArgs {
    pub execution_mode: ExecutionMode,
    pub arguments: Vec<String>,
}

impl Default for ShellArgs {
    fn default() -> Self {
        ShellArgs {
            execution_mode: ExecutionMode::Interactive,
            arguments: Vec::new(),
        }
    }
}

pub fn parse_args(
    mut args: Vec<String>,
    is_attached_to_terminal: bool,
) -> Result<ShellArgs, String> {
    if args.len() < 2 {
        return if is_attached_to_terminal {
            Ok(ShellArgs {
                execution_mode: ExecutionMode::Interactive,
                arguments: Vec::new(),
            })
        } else {
            Ok(ShellArgs {
                execution_mode: ExecutionMode::ReadCommandsFromStdin,
                arguments: Vec::new(),
            })
        };
    }

    match args[1].as_str() {
        "-c" => {
            let command_string = args
                .get(2)
                .cloned()
                .ok_or("-c requires an argument".to_string())?;
            args.drain(0..=2);
            Ok(ShellArgs {
                execution_mode: ExecutionMode::ReadCommandsFromString(command_string),
                arguments: args,
            })
        }
        "-s" => {
            args.drain(0..2);
            Ok(ShellArgs {
                execution_mode: ExecutionMode::ReadCommandsFromStdin,
                arguments: args,
            })
        }
        _ => {
            todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_args(args: Vec<&str>, is_interactive: bool) -> ShellArgs {
        let args = args.iter().map(|s| s.to_string()).collect();
        super::parse_args(args, is_interactive).expect("could not parse args")
    }

    #[test]
    fn read_commands_from_string_no_args() {
        let parsed_args = parse_args(vec!["sh", "-c", "ls -la"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromString("ls -la".to_string())
        );
        assert!(parsed_args.arguments.is_empty());

        assert_eq!(parsed_args, parse_args(vec!["sh", "-c", "ls -la"], true));
    }

    #[test]
    fn read_commands_from_string_with_args() {
        let parsed_args = parse_args(vec!["sh", "-c", "ls -la", "arg1", "arg2"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromString("ls -la".to_string())
        );
        assert_eq!(parsed_args.arguments, vec!["arg1", "arg2"]);
        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "-c", "ls -la", "arg1", "arg2"], true)
        );
    }

    #[test]
    fn read_commands_from_stdin_no_args() {
        let parsed_args = parse_args(vec!["sh", "-s"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin,
        );
        assert!(parsed_args.arguments.is_empty());

        assert_eq!(parsed_args, parse_args(vec!["sh", "-s"], true));
    }

    #[test]
    fn read_commands_from_stdin_with_args() {
        let parsed_args = parse_args(vec!["sh", "-s", "arg1", "arg2"], false);
        assert_eq!(
            parsed_args.execution_mode,
            ExecutionMode::ReadCommandsFromStdin
        );
        assert_eq!(parsed_args.arguments, vec!["arg1", "arg2"]);

        assert_eq!(
            parsed_args,
            parse_args(vec!["sh", "-s", "arg1", "arg2"], true)
        );
    }
}
