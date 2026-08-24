//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Ex command parser.

use super::address::{parse_address_range, parse_address_with_offset, Address, AddressRange};
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
                    // Anything else (`$`, `.+2`, `'a`, `/re/`) needs the buffer
                    // to resolve, so hand the range to the executor rather than
                    // refusing: `:$` on its own is a perfectly ordinary command.
                    return Ok(ExCommand::GotoAddress { range });
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
    let (cmd_name, force, raw_args) = split_command(rest);
    let args = raw_args.trim_start();
    let cmd_name = cmd_name.to_lowercase();

    if force && !accepts_force(&cmd_name) {
        return Err(ViError::InvalidCommand(format!("{cmd_name}!")));
    }

    match cmd_name.as_str() {
        // Write commands
        "w" | "write" => parse_write(range, args, force),
        "wq" => parse_write_quit(range, args, force, false),
        "x" | "xit" => parse_write_quit(range, args, force, true),

        // Quit commands
        "q" | "quit" => Ok(ExCommand::Quit { force }),

        // Edit commands: `e[dit][!][+command][file]` (94946).
        "e" | "edit" => {
            let (command, file) = split_plus_command(args);
            Ok(ExCommand::Edit {
                file: if file.is_empty() {
                    None
                } else {
                    Some(file.to_string())
                },
                force,
                command,
            })
        }

        // Read command
        //
        // `read` is one of the three commands (with `write` and `!`) whose `!`
        // is not a modifier at all (94854-94857): it marks the rest of the line
        // as a program to run rather than a file to read, whether or not a
        // <blank> separates it from the command name. A <backslash> escape
        // suppresses that meaning (95285-95286).
        "r" | "read" => {
            let shell_command = if force {
                Some(args)
            } else {
                args.strip_prefix('!')
            };
            if let Some(cmd) = shell_command {
                Ok(ExCommand::ShellRead {
                    range,
                    command: cmd.trim().to_string(),
                })
            } else {
                // The <backslash> has exactly one job here: suppressing the
                // `!`-means-command reading (95285-95286). Stripping it from
                // every argument renamed ordinary relative paths, so `:r \tmp/x`
                // looked for `tmp/x` and reported a path the user never typed.
                let file = match args.strip_prefix('\\') {
                    Some(rest) if rest.starts_with('!') => rest,
                    _ => args,
                };
                Ok(ExCommand::Read {
                    range,
                    file: if file.is_empty() {
                        None
                    } else {
                        Some(file.to_string())
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
            Ok(ExCommand::Put { range, register })
        }

        // Copy command
        "co" | "copy" | "t" => {
            let dest = parse_destination(args)?;
            Ok(ExCommand::Copy { range, dest })
        }

        // Move command
        "m" | "move" => {
            let dest = parse_destination(args)?;
            Ok(ExCommand::Move { range, dest })
        }

        // Substitute command
        "s" | "substitute" => parse_substitute(range, args),

        // Global commands. `g!` is the invert form, the same as `v`.
        "g" | "global" => parse_global(range, args, force),
        "v" | "vglobal" => parse_global(range, args, true),

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
            force,
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
            Ok(ExCommand::Mark { range, name })
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
        "cd" | "chd" | "chdir" => Ok(ExCommand::Cd {
            path: if args.is_empty() {
                None
            } else {
                Some(args.to_string())
            },
            force,
        }),
        "pwd" => Ok(ExCommand::Pwd),

        // Arg list commands: `n[ext][!][+command][file ...]` (95177). The file
        // list, when given, replaces the argument list (95181-95184); it used
        // to be discarded entirely, so `:n a b` was a plain `:n`.
        "n" | "next" => {
            let (command, rest) = split_plus_command(args);
            Ok(ExCommand::Next {
                force,
                files: split_escaped_fields(rest),
                command,
            })
        }
        "prev" | "previous" => Ok(ExCommand::Previous { force }),
        "rew" | "rewind" => Ok(ExCommand::Rewind { force }),
        "ar" | "args" => Ok(ExCommand::Args),

        // Undo/Redo
        "u" | "undo" => Ok(ExCommand::Undo),
        "red" | "redo" => Ok(ExCommand::Redo),

        // Mapping commands. Here `!` selects text input mode rather than
        // forcing anything (95090-95092).
        "map" => parse_map(args, MapMode::for_bang(force)),
        "unm" | "unmap" => Ok(ExCommand::Unmap {
            lhs: args.split_whitespace().next().unwrap_or("").to_string(),
            mode: MapMode::for_bang(force),
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
            force,
        }),
        "po" | "pop" => Ok(ExCommand::Pop),
        "tags" => Ok(ExCommand::Tags),

        // Info commands
        "ve" | "version" => Ok(ExCommand::Version),
        "h" | "help" => Ok(ExCommand::Help),

        // Recovery
        "pre" | "preserve" => Ok(ExCommand::Preserve),
        "rec" | "recover" => Ok(ExCommand::Recover {
            file: if args.is_empty() {
                None
            } else {
                Some(args.to_string())
            },
            force,
        }),

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

        // Text input commands. `!` toggles the autoindent edit option for the
        // duration of the command (94894-94896, 94910-94912, 95034-95036).
        "a" | "append" => Ok(ExCommand::Append {
            range,
            toggle_autoindent: force,
        }),
        "i" | "insert" => Ok(ExCommand::Insert {
            range,
            toggle_autoindent: force,
        }),
        "c" | "change" => Ok(ExCommand::Change {
            range,
            count: parse_optional_count(args),
            toggle_autoindent: force,
        }),

        // Visual and open mode commands.
        //
        // `vi[sual]` has two synopses: in open or visual mode it behaves as
        // `edit` (95473-95474), otherwise it is
        // `[1addr] vi[sual][type][count][flags]` (95472). Only the executor
        // knows which mode is current, so the arguments are carried through
        // unparsed rather than guessed at here — `+` is both a `+command`
        // introducer and a window type character.
        "vi" | "visual" => Ok(ExCommand::Visual {
            range,
            force,
            args: args.to_string(),
        }),
        "o" | "open" => {
            let pattern = parse_open_pattern(args);
            Ok(ExCommand::Open { range, pattern })
        }

        // Adjust window (z command)
        "z" => {
            let (ztype, type_count, count) = parse_z_args(raw_args)?;
            Ok(ExCommand::Z {
                range,
                ztype,
                type_count,
                count,
                full_screen: force,
            })
        }

        // Shift left/right
        "<" => Ok(ExCommand::ShiftLeft {
            range,
            count: parse_optional_count(args),
        }),
        ">" => Ok(ExCommand::ShiftRight {
            range,
            count: parse_optional_count(args),
        }),

        // Write line number
        "=" => Ok(ExCommand::LineNumber { range }),

        // Execute buffer
        "@" | "*" => {
            // `@@` (and `@*`) mean "repeat the last executed buffer", so they
            // must resolve to None here and let the executor fall back to
            // `last_macro_register`. Treating '@' as a buffer *name* made
            // `@@` fail with `Buffer "@" is empty`.
            let buffer = args.chars().next().filter(|c| c.is_ascii_alphabetic());
            Ok(ExCommand::Execute { range, buffer })
        }

        // Suspend
        "su" | "sus" | "suspend" | "st" | "stop" => Ok(ExCommand::Suspend { force }),

        // Repeat substitute (&)
        "&" => {
            let flags = SubstituteFlags::parse(args);
            Ok(ExCommand::RepeatSubstitute { range, flags })
        }

        // Repeat substitute with the LAST RE rather than the previous
        // substitute's pattern (#X18). `&` reuses both pattern and
        // replacement; `~` reuses the replacement but takes the pattern from
        // the most recent RE, whether that came from a search or a substitute.
        "~" => {
            let (flags_str, line_count) = split_subst_flags(args);
            let mut flags = SubstituteFlags::parse(&flags_str);
            flags.line_count = line_count;
            Ok(ExCommand::TildeSubstitute { range, flags })
        }

        // Print with line numbers (#) - alias for number
        "#" => Ok(ExCommand::Number {
            range,
            count: parse_optional_count(args),
        }),

        _ => Err(ViError::InvalidCommand(cmd_name)),
    }
}

/// Split a command name from its arguments, pulling off a trailing `!`.
///
/// POSIX spells the bang as part of each synopsis that accepts it
/// (`a[ppend][!]`, `w[rite][!]`, `z[!]`, ...) and states the general rule at
/// 94854-94857: "a character that can be appended to the command name to modify
/// its operation ... the '!' character shall only act as a modifier if there is
/// no <blank> between it and the command name". Returning it separately is what
/// enforces the adjacency rule in one place; treating it as part of the *name*
/// meant every forced form needed its own literal match arm — `"w!"`, `"q!"`,
/// `"e!"`, `"j!"`, ... — and the commands never given one silently swallowed the
/// bang instead of honouring or rejecting it.
fn split_command(input: &str) -> (&str, bool, &str) {
    // Special case: single-character commands
    let first_char = input.chars().next();
    match first_char {
        Some('!') => return ("!", false, input[1..].trim_start()),
        Some('<') => return ("<", false, input[1..].trim_start()),
        Some('>') => return (">", false, input[1..].trim_start()),
        Some('=') => return ("=", false, input[1..].trim_start()),
        Some('@') => return ("@", false, input[1..].trim_start()),
        Some('*') => return ("*", false, input[1..].trim_start()),
        Some('&') => return ("&", false, input[1..].trim_start()),
        Some('~') => return ("~", false, input[1..].trim_start()),
        Some('#') => return ("#", false, input[1..].trim_start()),
        _ => {}
    }

    // Find end of command name (letters only)
    let name_end = input
        .char_indices()
        .find(|(_, c)| !c.is_ascii_alphabetic())
        .map(|(i, _)| i)
        .unwrap_or(input.len());

    let cmd = &input[..name_end];
    let rest = &input[name_end..];
    let (force, rest) = match rest.strip_prefix('!') {
        Some(after) => (true, after),
        None => (false, rest),
    };
    // Returned untrimmed: `z` has to be able to tell whether a <blank>
    // separated its type argument from the command name (95554-95555).
    (cmd, force, rest)
}

/// Whether `cmd` accepts a `!` modifier.
///
/// The list is POSIX's, taken from the synopses that spell `[!]`, plus the
/// `prev[ious]` extension this editor already supported. Anything else with a
/// bang is a syntax error rather than a silently-ignored modifier.
fn accepts_force(cmd: &str) -> bool {
    matches!(
        cmd,
        "a" | "append"
            | "c"
            | "change"
            | "cd"
            | "chdir"
            | "chd"
            | "e"
            | "edit"
            | "g"
            | "global"
            | "i"
            | "insert"
            | "j"
            | "join"
            | "map"
            | "n"
            | "next"
            | "prev"
            | "previous"
            | "q"
            | "quit"
            | "r"
            | "read"
            | "rec"
            | "recover"
            | "rew"
            | "rewind"
            | "st"
            | "stop"
            | "su"
            | "sus"
            | "suspend"
            | "ta"
            | "tag"
            | "unm"
            | "unmap"
            | "vi"
            | "visual"
            | "w"
            | "write"
            | "wq"
            | "x"
            | "xit"
            | "z"
    )
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
fn parse_write_quit(range: AddressRange, args: &str, force: bool, xit: bool) -> Result<ExCommand> {
    let file = if args.is_empty() {
        None
    } else {
        Some(args.to_string())
    };
    Ok(ExCommand::WriteQuit {
        range,
        file,
        force,
        xit,
    })
}

/// Parse substitute command.
fn parse_substitute(range: AddressRange, args: &str) -> Result<ExCommand> {
    // s[/pattern/replacement/][flags][count]
    //
    // A bare `s`, and an empty pattern (`s//repl/`), both mean "reuse the last
    // regular expression" (ex.md §95700). Both used to be rejected outright as
    // NoPreviousSubstitution, which is a decision only the editor can make --
    // it is the one that knows whether a previous RE exists. An empty
    // `pattern` is therefore passed through and resolved at execution time.
    let args = args.trim_start();

    // Bare `s`, optionally followed by flags/count: `s`, `s g`, `s 3`.
    if args.is_empty() || args.chars().next().is_some_and(|c| c.is_alphanumeric()) {
        let (flags, line_count) = split_subst_flags(args);
        let mut flags = SubstituteFlags::parse(&flags);
        flags.line_count = line_count;
        return Ok(ExCommand::Substitute {
            range,
            pattern: String::new(),
            replacement: "~".to_string(),
            flags,
        });
    }

    let delim = args.chars().next().unwrap();
    let parts: Vec<&str> = args[delim.len_utf8()..].split(delim).collect();

    let pattern = parts.first().unwrap_or(&"").to_string();
    let replacement = parts.get(1).unwrap_or(&"").to_string();
    let trailing = parts.get(2).unwrap_or(&"");
    let (flags_str, line_count) = split_subst_flags(trailing);
    let mut flags = SubstituteFlags::parse(&flags_str);
    flags.line_count = line_count;

    Ok(ExCommand::Substitute {
        range,
        pattern,
        replacement,
        flags,
    })
}

/// Split a substitute suffix into its flag letters and a trailing numeric
/// count, e.g. `"g3"` -> `("g", Some(3))`.
fn split_subst_flags(s: &str) -> (String, Option<usize>) {
    let s = s.trim();
    let digits_at = s
        .char_indices()
        .position(|(_, c)| c.is_ascii_digit())
        .and_then(|_| s.find(|c: char| c.is_ascii_digit()));
    match digits_at {
        Some(i) => {
            let (flags, num) = s.split_at(i);
            (flags.trim().to_string(), num.trim().parse().ok())
        }
        None => (s.to_string(), None),
    }
}

/// Parse global command.
fn parse_global(range: AddressRange, args: &str, invert: bool) -> Result<ExCommand> {
    // g/pattern/command
    if args.is_empty() {
        return Err(ViError::InvalidCommand(
            "global requires pattern".to_string(),
        ));
    }

    // The delimiter is any character, so step over it by its encoded width --
    // `[1..]` slices through a multi-byte delimiter and panics.
    let delim = args.chars().next().unwrap();
    let rest = &args[delim.len_utf8()..];

    // Find end of pattern
    let pattern_end = rest.find(delim).unwrap_or(rest.len());
    let pattern = rest[..pattern_end].to_string();
    let command = if pattern_end < rest.len() {
        rest[pattern_end + delim.len_utf8()..].to_string()
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

/// Parse the destination of `:copy`/`:move`.
///
/// POSIX (ex, `copy`/`move`) gives it as an *address*, so `$`, `.`, `.+2`, a
/// mark and a search are all valid.  Parsing it as a bare integer rejected
/// everything but a decimal literal -- `:1m$` answered "invalid line number".
/// It stays unresolved until execution, since `$` depends on the buffer.
fn parse_destination(args: &str) -> Result<Address> {
    let trimmed = args.trim();
    if trimmed.is_empty() {
        return Err(ViError::InvalidAddress(
            "missing destination address".to_string(),
        ));
    }
    match parse_address_with_offset(trimmed) {
        Some((addr, rest)) if rest.trim().is_empty() => Ok(addr),
        _ => Err(ViError::InvalidAddress(format!(
            "invalid destination: {}",
            trimmed
        ))),
    }
}

/// Split a leading `+command` argument from `args`.
///
/// "The +command option shall be <blank>-delimited; <blank> characters within
/// the +command can be escaped by preceding them with a <backslash> character"
/// (94954-94955). Returns the command with those escapes removed, and the rest
/// of the argument string.
pub(crate) fn split_plus_command(args: &str) -> (Option<String>, &str) {
    let Some(body) = args.strip_prefix('+') else {
        return (None, args);
    };
    let (mut cmd, rest) = take_escaped_field(body);
    // A bare `+` is the historical "start at the last line".
    if cmd.is_empty() {
        cmd.push('$');
    }
    (Some(cmd), rest)
}

/// Take one <blank>-delimited field from the front of `s`, removing the
/// <backslash> escapes that protect a <blank> from ending it (94954-94955).
///
/// Returns the unescaped field and the remainder with leading <blank>s removed.
fn take_escaped_field(s: &str) -> (String, &str) {
    let mut field = String::new();
    let mut escaped = false;
    let mut end = s.len();
    for (i, c) in s.char_indices() {
        if escaped {
            field.push(c);
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c.is_whitespace() {
            end = i;
            break;
        } else {
            field.push(c);
        }
    }
    (field, s[end..].trim_start())
}

/// Split `s` into <blank>-delimited fields, honouring the same <backslash>
/// escapes as [`split_plus_command`].
///
/// `str::split_whitespace` would tear `my\ file.txt` into two operands and leave
/// the backslash in the first, so the argument list `:next` builds named files
/// that do not exist.
fn split_escaped_fields(s: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut rest = s.trim_start();
    while !rest.is_empty() {
        let (field, tail) = take_escaped_field(rest);
        if !field.is_empty() {
            fields.push(field);
        }
        rest = tail;
    }
    fields
}

/// Parse the `/pattern/` argument of the `open` command (95212-95216).
///
/// The trailing delimiter may be omitted, an empty pattern means "the last
/// regular expression used in the editor", and the delimiter may be any
/// alphanumeric or non-<blank> other than <backslash>, <vertical-line>,
/// <newline> or double-quote.
fn parse_open_pattern(args: &str) -> Option<String> {
    let args = args.trim();
    let mut chars = args.chars();
    let delim = chars.next()?;
    if matches!(delim, '\\' | '|' | '"' | '\n') || delim.is_whitespace() {
        return None;
    }
    let rest = &args[delim.len_utf8()..];
    let pattern = match rest.find(delim) {
        Some(end) => &rest[..end],
        // "The trailing delimiter can be omitted ... at the end of the command
        // line."
        None => rest,
    };
    // "If pattern is empty (for example, "//") ... the last regular expression
    // used in the editor shall be used", which the executor signals with None.
    if pattern.is_empty() {
        None
    } else {
        Some(pattern.to_string())
    }
}

/// Parse `z` arguments: the run of type characters, then an optional count.
///
/// Returns `(type character, how many of it were given, count)`. The count of
/// type characters matters: POSIX defines `-` and `^` as decrementing by
/// `((number of characters) x count) - 1` and `+` as incrementing by
/// `((number of characters) - 1) x count + 1` (95562-95592), so `z--` and `z++`
/// are not the same as `z-` and `z+`. Only the first character used to be read,
/// which made every repeat a no-op.
///
/// "If there are <blank> characters between the type argument and the preceding
/// z command name or optional '!' character, it shall be an error" (95554-95555).
fn parse_z_args(args: &str) -> Result<(Option<char>, usize, Option<usize>)> {
    const TYPES: [char; 5] = ['+', '-', '.', '=', '^'];

    // "If there are <blank> characters between the type argument and the
    // preceding z command name or optional '!' character, it shall be an error"
    // (95554-95555). This is why `args` arrives untrimmed. A <blank> before a
    // *count* is still legal -- the rule names only the type argument.
    let trimmed = args.trim_start();
    if trimmed.len() != args.len() && trimmed.starts_with(TYPES) {
        return Err(ViError::InvalidCommand(
            "z: no <blank> may precede the type character".into(),
        ));
    }
    let args = trimmed;

    if args.is_empty() {
        return Ok((None, 0, None));
    }
    let first = args.chars().next().unwrap();
    if !TYPES.contains(&first) {
        // No type: the whole argument is the count.
        return Ok((None, 0, args.trim().parse().ok()));
    }

    let repeats = args.chars().take_while(|c| *c == first).count();
    // "If more than a single '.' or '=' is specified, it shall be an error."
    if repeats > 1 && (first == '.' || first == '=') {
        return Err(ViError::InvalidCommand(format!(
            "z: {first} may not be repeated"
        )));
    }
    let rest = &args[repeats * first.len_utf8()..];
    // A different type character following the run is a malformed type.
    if rest.chars().next().is_some_and(|c| TYPES.contains(&c)) {
        return Err(ViError::InvalidCommand("z: mixed type characters".into()));
    }
    Ok((Some(first), repeats, rest.trim().parse().ok()))
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
        if let ExCommand::Edit { file, force, .. } = cmd {
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

    /// `g` may be delimited by any character, including a multi-byte one.
    /// `parse_substitute` already steps by `len_utf8()`; `parse_global` used a
    /// bare `[1..]`, which slices through a multi-byte delimiter and panics.
    #[test]
    fn test_parse_global_multibyte_delimiter() {
        let cmd = parse_ex_command("g\u{b5}foo\u{b5}d").unwrap();
        if let ExCommand::Global {
            pattern, command, ..
        } = cmd
        {
            assert_eq!(pattern, "foo");
            assert_eq!(command, "d");
        } else {
            panic!("Expected Global command");
        }
    }
}
