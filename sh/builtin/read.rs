//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{args_as_str, BuiltinError, BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::os::errno::Errno;
use crate::os::read;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::shstr::ShString;
use crate::wordexp::expanded_word::ExpandedWord;
use crate::wordexp::split_fields;
use gettextrs::gettext;
use std::io::IsTerminal;
use std::os::fd::{AsRawFd, RawFd};

/// The diagnostic for input `read` could not read (`read <&1` with standard
/// output a pipe gives EBADF): the system's reason.
fn read_error(errno: Errno) -> BuiltinError {
    format!(
        "read: {}: {}",
        gettext("read error"),
        plib::diag::io_error_text(&std::io::Error::from(errno))
    )
    .into()
}

fn read_byte_non_blocking(fd: RawFd) -> Result<Option<u8>, BuiltinError> {
    let mut buffer = [0u8; 1];
    match read(fd, &mut buffer) {
        Ok(0) => Ok(None),
        Ok(1) => Ok(Some(buffer[0])),
        Ok(_) => unreachable!(),
        Err(err) if err.errno == Errno::EAGAIN => Ok(None),
        Err(err) => Err(read_error(err.errno)),
    }
}

/// Reads one byte, or None at the end of the input or once an interrupt
/// (^C in an interactive shell) abandons the command. A signal that
/// interrupts the read has its trap run, and the read goes on, as when
/// reading from the terminal.
fn read_byte(shell: &mut Shell, fd: RawFd) -> Result<Option<u8>, BuiltinError> {
    let mut buffer = [0u8; 1];
    loop {
        match read(fd, &mut buffer) {
            Ok(0) => return Ok(None),
            Ok(1) => return Ok(Some(buffer[0])),
            Ok(_) => unreachable!(),
            Err(err) if err.errno == Errno::EINTR => {
                shell.handle_async_events();
                if shell.is_interrupted() {
                    return Ok(None);
                }
            }
            Err(err) => return Err(read_error(err.errno)),
        }
    }
}

/// The `PS2` continuation prompt, written through the shell's own standard
/// error so that a redirection of the built-in's stderr is honored.
#[derive(Clone, Copy)]
struct Prompt<'a> {
    prompt: &'a str,
    opened_files: &'a OpenedFiles,
}

impl Prompt<'_> {
    fn write(&self) {
        self.opened_files.write_err(self.prompt);
    }
}

/// Whether `byte` is dropped from the input: a NUL, unless it is the
/// delimiter (`-d ''`). As in dash and bash (POSIX leaves input holding a
/// NUL unspecified): no variable can hold one, and it is dropped before
/// anything else looks at it, so a backslash before a NUL escapes the
/// byte after it, as in dash.
fn is_dropped(byte: u8, delimiter: u8) -> bool {
    byte == 0 && delimiter != 0
}

struct ReadResult {
    contents: ExpandedWord,
    reached_eof: bool,
}

fn read_until_from_non_blocking_fd(
    shell: &mut Shell,
    fd: RawFd,
    delimiter: u8,
    backslash_escape: bool,
    continuation_prompt: Option<Prompt<'_>>,
) -> Result<ReadResult, BuiltinError> {
    let mut buffer = Vec::new();
    let mut result = ExpandedWord::default();
    let mut escape_next = false;
    'outer: loop {
        while let Some(next) = read_byte_non_blocking(fd)? {
            if is_dropped(next, delimiter) {
                continue;
            }
            if escape_next {
                escape_next = false;
                if next == delimiter {
                    // backslash-<delimiter>: line continuation.
                    if let Some(prompt) = &continuation_prompt {
                        prompt.write();
                    }
                    continue;
                } else if next == b'\\' {
                    buffer.push(b'\\');
                } else {
                    result.append(std::mem::take(&mut buffer), false, true);
                    result.append(vec![next], true, true);
                }
                continue;
            }
            if next == delimiter {
                break 'outer;
            }
            if backslash_escape && next == b'\\' {
                escape_next = true;
                continue;
            }
            buffer.push(next);
        }
        // might receive signals while reading
        shell.handle_async_events();
        if shell.is_interrupted() {
            break;
        }
        // Wait for the next byte instead of sleeping a tick between polls; the
        // signal pipe is watched too, so a trap is still handled promptly.
        crate::os::wait_for_input(fd)?;
    }
    if !buffer.is_empty() {
        result.append(buffer, false, true);
    }
    Ok(ReadResult {
        contents: result,
        reached_eof: false,
    })
}

fn read_until_from_file(
    shell: &mut Shell,
    fd: RawFd,
    delimiter: u8,
    backslash_escape: bool,
    continuation_prompt: Option<Prompt<'_>>,
) -> Result<ReadResult, BuiltinError> {
    let mut buffer = Vec::new();
    let mut result = ExpandedWord::default();
    let mut escape_next = false;
    let mut reached_eof = true;
    while let Some(next) = read_byte(shell, fd)? {
        if is_dropped(next, delimiter) {
            continue;
        }
        if escape_next {
            escape_next = false;
            if next == delimiter {
                // backslash-<delimiter> is a line continuation: drop both and
                // keep reading (the delimiter is usually <newline>).
                if let Some(prompt) = &continuation_prompt {
                    prompt.write();
                }
                continue;
            } else if next == b'\\' {
                buffer.push(b'\\');
            } else {
                result.append(std::mem::take(&mut buffer), false, true);
                result.append(vec![next], true, true);
            }
            continue;
        }
        if next == delimiter {
            reached_eof = false;
            break;
        }
        if backslash_escape && next == b'\\' {
            escape_next = true;
            continue;
        }
        buffer.push(next);
    }

    if !buffer.is_empty() {
        result.append(buffer, false, true);
    }
    Ok(ReadResult {
        contents: result,
        reached_eof,
    })
}

fn read_from_stdin(
    shell: &mut Shell,
    delimiter: u8,
    backslash_escape: bool,
    continuation_prompt: Option<Prompt<'_>>,
) -> Result<ReadResult, BuiltinError> {
    if std::io::stdin().is_terminal() {
        let original_terminal_settings = shell.terminal.reset();
        shell.terminal.set_nonblocking();

        let result = read_until_from_non_blocking_fd(
            shell,
            std::io::stdin().as_raw_fd(),
            delimiter,
            backslash_escape,
            continuation_prompt,
        );

        shell.terminal.set(original_terminal_settings);

        result
    } else {
        read_until_from_file(
            shell,
            std::io::stdin().as_raw_fd(),
            delimiter,
            backslash_escape,
            continuation_prompt,
        )
    }
}

/// Reads one line (up to `delimiter`) from the shell's standard input,
/// which the command's redirections, if any, have already replaced.
fn read_until(
    shell: &mut Shell,
    delimiter: u8,
    backslash_escape: bool,
    opened_files: &OpenedFiles,
) -> Result<ReadResult, BuiltinError> {
    // PS2 is written for each line continuation `read` consumes (only possible
    // without -r), but only when the shell is interactive *and* the input is
    // actually the terminal -- prompting for text coming from a file would be
    // noise. It goes through the shell's stderr so redirection is honored.
    let prompt_ps2 = shell.is_interactive && backslash_escape && std::io::stdin().is_terminal();
    let continuation_prompt = if prompt_ps2 {
        Some(shell.get_ps2())
    } else {
        None
    };
    let continuation_prompt = continuation_prompt.as_deref().map(|prompt| Prompt {
        prompt,
        opened_files,
    });
    read_from_stdin(shell, delimiter, backslash_escape, continuation_prompt)
}

pub struct BuiltinRead;

impl BuiltinUtility for BuiltinRead {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = &args_as_str("read", args)?;
        let mut option_parser = OptionParser::new(args);

        let mut delim = None;
        let mut backslash_escape = true;
        while let Some(option) = option_parser
            .next_option()
            .map_err(|arg| format!("read: invalid option: {arg}"))?
        {
            match option {
                'r' => {
                    backslash_escape = false;
                }
                'd' => {
                    if delim.is_some() {
                        return Err(gettext("read: -d can only be specified once").into());
                    }
                    let arg = option_parser
                        .next_option_argument()
                        .ok_or("read: -d requires an argument")?;
                    if arg.len() > 1 {
                        return Err(gettext("read: -d requires a single character").into());
                    }
                    if arg.is_empty() {
                        delim = Some(b'\0');
                    } else {
                        delim = Some(arg.as_bytes()[0]);
                    }
                }
                other => return Err(format!("read: invalid option -{other}").into()),
            }
        }

        let first_operand = option_parser.next_argument();
        if first_operand == args.len() {
            return Err(gettext("read: missing operand").into());
        }
        let vars = &args[first_operand..];

        let input = read_until(
            shell,
            delim.unwrap_or(b'\n'),
            backslash_escape,
            opened_files,
        )?;
        // An interrupted `read` assigns nothing (dash).
        if shell.is_interrupted() {
            return Ok(128 + libc::SIGINT);
        }

        let fields = split_fields(
            input.contents,
            shell.environment.get_str_value("IFS"),
            vars.len(),
        );

        // POSIX EXIT STATUS: 1 means end-of-file, so an assignment failure has
        // to report something greater than 1.
        let mut assignment_failed = false;
        for i in 0..fields.len() {
            if shell
                .assign_global(vars[i].to_string(), fields[i].to_sh_string())
                .is_err()
            {
                opened_files.write_err(format!(
                    "read: {} {}\n",
                    gettext("cannot set readonly variable"),
                    vars[i]
                ));
                assignment_failed = true;
            }
        }
        if fields.len() < vars.len() {
            for var in &vars[fields.len()..] {
                if shell
                    .assign_global(var.to_string(), ShString::new())
                    .is_err()
                {
                    opened_files.write_err(format!(
                        "read: {} {}\n",
                        gettext("cannot set readonly variable"),
                        var
                    ));
                    assignment_failed = true;
                }
            }
        }
        if assignment_failed {
            return Ok(2);
        }

        if input.reached_eof {
            Ok(1)
        } else {
            Ok(0)
        }
    }
}
