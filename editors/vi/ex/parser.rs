//! Ex command parser.

use super::address::{parse_address_range, Address, AddressRange};
use super::command::{ExCommand, MapMode, SubstituteFlags};
use crate::error::{Result, ViError};

/// Parse an ex command string.
pub fn parse_ex_command(input: &str) -> Result<ExCommand> {
    let input = input.trim();
    if input.is_empty() {
        return Ok(ExCommand::Nop);
    }

    // Parse any address range
    let (range, rest) = parse_address_range(input);
    let rest = rest.trim_start();

    // If only an address with no command, it's a goto
    if rest.is_empty() {
        if range.explicit {
            match (&range.start, &range.end) {
                (Some(addr), None) => {
                    // Single address - go to that line
                    if let Address::Line(n) = addr {
                        return Ok(ExCommand::Goto { line: *n });
                    }
                    // For other address types, we need a buffer to resolve
                    return Err(ViError::InvalidCommand("unresolved address".to_string()));
                }
                (Some(_), Some(_)) => {
                    // Range specified but no command
                    return Err(ViError::InvalidCommand("no command specified".to_string()));
                }
                _ => return Ok(ExCommand::Nop),
            };
        }
        return Ok(ExCommand::Nop);
    }

    // Parse the command name
    let (cmd_name, args) = split_command(rest);
    let cmd_name = cmd_name.to_lowercase();

    match cmd_name.as_str() {
        // Write commands
        "w" | "write" => parse_write(range, args, false),
        "w!" => parse_write(range, args, true),
        "wq" => parse_write_quit(range, args, false),
        "wq!" => parse_write_quit(range, args, true),
        "x" | "xit" => parse_write_quit(range, args, false),

        // Quit commands
        "q" | "quit" => Ok(ExCommand::Quit { force: false }),
        "q!" | "quit!" => Ok(ExCommand::Quit { force: true }),

        // Edit commands
        "e" | "edit" => Ok(ExCommand::Edit {
            file: if args.is_empty() {
                None
            } else {
                Some(args.to_string())
            },
            force: false,
        }),
        "e!" | "edit!" => Ok(ExCommand::Edit {
            file: if args.is_empty() {
                None
            } else {
                Some(args.to_string())
            },
            force: true,
        }),

        // Read command
        "r" | "read" => {
            if let Some(cmd) = args.strip_prefix('!') {
                // Shell read
                Ok(ExCommand::ShellRead {
                    line: range.start.as_ref().and_then(|a| {
                        if let Address::Line(n) = a {
                            Some(*n)
                        } else {
                            None
                        }
                    }),
                    command: cmd.trim().to_string(),
                })
            } else {
                Ok(ExCommand::Read {
                    range,
                    file: if args.is_empty() {
                        None
                    } else {
                        Some(args.to_string())
                    },
                })
            }
        }

        // Delete command
        "d" | "delete" => {
            let (register, count) = parse_register_and_count(args);
            Ok(ExCommand::Delete {
                range,
                register,
                count,
            })
        }

        // Yank command
        "y" | "yank" => {
            let (register, count) = parse_register_and_count(args);
            Ok(ExCommand::Yank {
                range,
                register,
                count,
            })
        }

        // Put command
        "pu" | "put" => {
            let register = args.chars().next().filter(|c| c.is_ascii_alphabetic());
            Ok(ExCommand::Put {
                line: range.start.as_ref().and_then(|a| {
                    if let Address::Line(n) = a {
                        Some(*n)
                    } else {
                        None
                    }
                }),
                register,
            })
        }

        // Copy command
        "co" | "copy" | "t" => {
            let dest = parse_line_number(args)?;
            Ok(ExCommand::Copy { range, dest })
        }

        // Move command
        "m" | "move" => {
            let dest = parse_line_number(args)?;
            Ok(ExCommand::Move { range, dest })
        }

        // Substitute command
        "s" | "substitute" => parse_substitute(range, args),

        // Global commands
        "g" | "global" => parse_global(range, args, false),
        "g!" | "v" | "vglobal" => parse_global(range, args, true),

        // Print commands
        "p" | "print" => Ok(ExCommand::Print {
            range,
            count: parse_optional_count(args),
        }),
        "nu" | "number" => Ok(ExCommand::Number {
            range,
            count: parse_optional_count(args),
        }),
        "l" | "list" => Ok(ExCommand::List {
            range,
            count: parse_optional_count(args),
        }),

        // Join command
        "j" | "join" => Ok(ExCommand::Join {
            range,
            count: parse_optional_count(args),
        }),

        // Set command
        "se" | "set" => Ok(ExCommand::Set {
            args: args.to_string(),
        }),

        // File info
        "f" | "file" => Ok(ExCommand::File {
            new_name: if args.is_empty() {
                None
            } else {
                Some(args.to_string())
            },
        }),

        // Mark command
        "ma" | "mark" | "k" => {
            let name = args
                .chars()
                .next()
                .ok_or(ViError::InvalidCommand("mark name required".to_string()))?;
            Ok(ExCommand::Mark {
                line: range.start.as_ref().and_then(|a| {
                    if let Address::Line(n) = a {
                        Some(*n)
                    } else {
                        None
                    }
                }),
                name,
            })
        }

        // Shell command
        "!" => {
            if range.explicit {
                // Range specified - this is a filter command
                Ok(ExCommand::ShellFilter {
                    range,
                    command: args.to_string(),
                })
            } else {
                // No range - simple shell escape
                Ok(ExCommand::Shell {
                    command: args.to_string(),
                })
            }
        }
        "sh" | "shell" => Ok(ExCommand::Shell {
            command: String::new(),
        }),

        // Directory commands
        "cd" | "chdir" => Ok(ExCommand::Cd {
            path: if args.is_empty() {
                None
            } else {
                Some(args.to_string())
            },
        }),
        "pwd" => Ok(ExCommand::Pwd),

        // Arg list commands
        "n" | "next" => Ok(ExCommand::Next { force: false }),
        "n!" | "next!" => Ok(ExCommand::Next { force: true }),
        "prev" | "previous" => Ok(ExCommand::Previous { force: false }),
        "prev!" | "previous!" => Ok(ExCommand::Previous { force: true }),
        "rew" | "rewind" => Ok(ExCommand::Rewind { force: false }),
        "rew!" | "rewind!" => Ok(ExCommand::Rewind { force: true }),
        "ar" | "args" => Ok(ExCommand::Args),

        // Undo/Redo
        "u" | "undo" => Ok(ExCommand::Undo),
        "red" | "redo" => Ok(ExCommand::Redo),

        // Mapping commands
        "map" => parse_map(args, MapMode::Command),
        "map!" => parse_map(args, MapMode::Insert),
        "unmap" => Ok(ExCommand::Unmap {
            lhs: args.split_whitespace().next().unwrap_or("").to_string(),
            mode: MapMode::Command,
        }),
        "unmap!" => Ok(ExCommand::Unmap {
            lhs: args.split_whitespace().next().unwrap_or("").to_string(),
            mode: MapMode::Insert,
        }),

        // Abbreviations
        "ab" | "abbreviate" => {
            let (lhs, rhs) = split_first_word(args);
            Ok(ExCommand::Abbreviate {
                lhs: lhs.to_string(),
                rhs: rhs.to_string(),
            })
        }
        "una" | "unabbreviate" => Ok(ExCommand::Unabbreviate {
            lhs: args.split_whitespace().next().unwrap_or("").to_string(),
        }),

        // Tag commands
        "ta" | "tag" => Ok(ExCommand::Tag {
            tag: args.to_string(),
        }),
        "po" | "pop" => Ok(ExCommand::Pop),
        "tags" => Ok(ExCommand::Tags),

        // Info commands
        "ve" | "version" => Ok(ExCommand::Version),
        "h" | "help" => Ok(ExCommand::Help),

        // Source file
        "so" | "source" => {
            if args.is_empty() {
                Err(ViError::InvalidCommand(
                    "source requires a filename".to_string(),
                ))
            } else {
                Ok(ExCommand::Source {
                    file: args.to_string(),
                })
            }
        }

        // Text input commands
        "a" | "append" => {
            let line = range
                .start
                .as_ref()
                .and_then(|a| {
                    if let Address::Line(n) = a {
                        Some(*n)
                    } else {
                        None
                    }
                })
                .unwrap_or(1); // Default to current line (will be resolved later)
            Ok(ExCommand::Append { line })
        }
        "i" | "insert" => {
            let line = range
                .start
                .as_ref()
                .and_then(|a| {
                    if let Address::Line(n) = a {
                        Some(*n)
                    } else {
                        None
                    }
                })
                .unwrap_or(1);
            Ok(ExCommand::Insert { line })
        }
        "c" | "change" => Ok(ExCommand::Change { range }),

        _ => Err(ViError::InvalidCommand(cmd_name)),
    }
}

/// Split command name from arguments.
fn split_command(input: &str) -> (&str, &str) {
    // Special case: ! is a single-character command
    if let Some(rest) = input.strip_prefix('!') {
        return ("!", rest.trim_start());
    }

    // Find end of command name (letters only, or special chars like !)
    let cmd_end = input
        .char_indices()
        .find(|(_, c)| !c.is_ascii_alphabetic() && *c != '!')
        .map(|(i, _)| i)
        .unwrap_or(input.len());

    let cmd = &input[..cmd_end];
    let args = input[cmd_end..].trim_start();
    (cmd, args)
}

/// Split first word from rest.
fn split_first_word(input: &str) -> (&str, &str) {
    let input = input.trim_start();
    let end = input
        .char_indices()
        .find(|(_, c)| c.is_whitespace())
        .map(|(i, _)| i)
        .unwrap_or(input.len());

    (&input[..end], input[end..].trim_start())
}

/// Parse write command.
fn parse_write(range: AddressRange, args: &str, force: bool) -> Result<ExCommand> {
    let args = args.trim();
    if let Some(rest) = args.strip_prefix(">>") {
        // Append mode
        let f = rest.trim();
        let file = if f.is_empty() {
            None
        } else {
            Some(f.to_string())
        };
        return Ok(ExCommand::Write {
            range,
            file,
            append: true,
            force,
        });
    }
    if let Some(cmd) = args.strip_prefix('!') {
        // Write to shell command
        return Ok(ExCommand::ShellWrite {
            range,
            command: cmd.trim().to_string(),
        });
    }
    let file = if args.is_empty() {
        None
    } else {
        Some(args.to_string())
    };
    Ok(ExCommand::Write {
        range,
        file,
        append: false,
        force,
    })
}

/// Parse write-quit command.
fn parse_write_quit(range: AddressRange, args: &str, force: bool) -> Result<ExCommand> {
    let file = if args.is_empty() {
        None
    } else {
        Some(args.to_string())
    };
    Ok(ExCommand::WriteQuit { range, file, force })
}

/// Parse substitute command.
fn parse_substitute(range: AddressRange, args: &str) -> Result<ExCommand> {
    // s/pattern/replacement/flags
    if args.is_empty() {
        return Err(ViError::NoPreviousSubstitution);
    }

    let delim = args.chars().next().unwrap();
    let parts: Vec<&str> = args[1..].split(delim).collect();

    let pattern = parts.first().unwrap_or(&"").to_string();
    let replacement = parts.get(1).unwrap_or(&"").to_string();
    let flags_str = parts.get(2).unwrap_or(&"");
    let flags = SubstituteFlags::parse(flags_str);

    if pattern.is_empty() {
        return Err(ViError::NoPreviousSubstitution);
    }

    Ok(ExCommand::Substitute {
        range,
        pattern,
        replacement,
        flags,
    })
}

/// Parse global command.
fn parse_global(range: AddressRange, args: &str, invert: bool) -> Result<ExCommand> {
    // g/pattern/command
    if args.is_empty() {
        return Err(ViError::InvalidCommand(
            "global requires pattern".to_string(),
        ));
    }

    let delim = args.chars().next().unwrap();
    let rest = &args[1..];

    // Find end of pattern
    let pattern_end = rest.find(delim).unwrap_or(rest.len());
    let pattern = rest[..pattern_end].to_string();
    let command = if pattern_end < rest.len() {
        rest[pattern_end + 1..].to_string()
    } else {
        "p".to_string() // Default command is print
    };

    Ok(ExCommand::Global {
        range,
        pattern,
        command,
        invert,
    })
}

/// Parse map command.
fn parse_map(args: &str, mode: MapMode) -> Result<ExCommand> {
    let (lhs, rhs) = split_first_word(args);
    Ok(ExCommand::Map {
        lhs: lhs.to_string(),
        rhs: rhs.to_string(),
        mode,
    })
}

/// Parse register and count from args.
fn parse_register_and_count(args: &str) -> (Option<char>, Option<usize>) {
    let args = args.trim();
    if args.is_empty() {
        return (None, None);
    }

    let first = args.chars().next().unwrap();
    if first.is_ascii_alphabetic() {
        let count = args[1..].trim().parse().ok();
        (Some(first), count)
    } else {
        let count = args.parse().ok();
        (None, count)
    }
}

/// Parse optional count.
fn parse_optional_count(args: &str) -> Option<usize> {
    args.trim().parse().ok()
}

/// Parse a line number.
fn parse_line_number(args: &str) -> Result<usize> {
    args.trim()
        .parse()
        .map_err(|_| ViError::InvalidAddress("invalid line number".to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_quit() {
        let cmd = parse_ex_command("q").unwrap();
        assert!(matches!(cmd, ExCommand::Quit { force: false }));

        let cmd = parse_ex_command("q!").unwrap();
        assert!(matches!(cmd, ExCommand::Quit { force: true }));
    }

    #[test]
    fn test_parse_write() {
        let cmd = parse_ex_command("w").unwrap();
        assert!(matches!(cmd, ExCommand::Write { force: false, .. }));

        let cmd = parse_ex_command("w foo.txt").unwrap();
        if let ExCommand::Write { file, .. } = cmd {
            assert_eq!(file, Some("foo.txt".to_string()));
        } else {
            panic!("Expected Write command");
        }
    }

    #[test]
    fn test_parse_edit() {
        let cmd = parse_ex_command("e newfile.txt").unwrap();
        if let ExCommand::Edit { file, force } = cmd {
            assert_eq!(file, Some("newfile.txt".to_string()));
            assert!(!force);
        } else {
            panic!("Expected Edit command");
        }
    }

    #[test]
    fn test_parse_substitute() {
        let cmd = parse_ex_command("s/foo/bar/g").unwrap();
        if let ExCommand::Substitute {
            pattern,
            replacement,
            flags,
            ..
        } = cmd
        {
            assert_eq!(pattern, "foo");
            assert_eq!(replacement, "bar");
            assert!(flags.global);
        } else {
            panic!("Expected Substitute command");
        }
    }

    #[test]
    fn test_parse_range_command() {
        let cmd = parse_ex_command("1,5d").unwrap();
        if let ExCommand::Delete { range, .. } = cmd {
            assert!(range.explicit);
        } else {
            panic!("Expected Delete command");
        }
    }

    #[test]
    fn test_parse_global() {
        let cmd = parse_ex_command("g/pattern/d").unwrap();
        if let ExCommand::Global {
            pattern,
            command,
            invert,
            ..
        } = cmd
        {
            assert_eq!(pattern, "pattern");
            assert_eq!(command, "d");
            assert!(!invert);
        } else {
            panic!("Expected Global command");
        }
    }

    #[test]
    fn test_parse_set() {
        let cmd = parse_ex_command("set number").unwrap();
        if let ExCommand::Set { args } = cmd {
            assert_eq!(args, "number");
        } else {
            panic!("Expected Set command");
        }
    }
}
