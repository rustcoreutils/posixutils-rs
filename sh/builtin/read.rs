//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinError, BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::os::errno::Errno;
use crate::os::read;
use crate::shell::opened_files::{OpenedFile, OpenedFiles, STDIN_FILENO};
use crate::shell::Shell;
use crate::wordexp::expanded_word::ExpandedWord;
use crate::wordexp::split_fields;
use gettextrs::gettext;
use std::io::IsTerminal;
use std::os::fd::{AsRawFd, RawFd};
use std::time::Duration;

fn bytes_to_string(bytes: Vec<u8>) -> Result<String, BuiltinError> {
    String::from_utf8(bytes.to_vec()).map_err(|_| gettext("read: invalid UTF-8").into())
}

fn read_byte_non_blocking(fd: RawFd) -> Result<Option<u8>, BuiltinError> {
    let mut buffer = [0u8; 1];
    match read(fd, &mut buffer) {
        Ok(0) => Ok(None),
        Ok(1) => Ok(Some(buffer[0])),
        Ok(_) => unreachable!(),
        Err(err) if err.errno == Errno::EAGAIN => Ok(None),
        Err(err) => Err(format!("read: failed to read from stdin ({err})").into()),
    }
}

fn read_byte(fd: RawFd) -> Result<Option<u8>, BuiltinError> {
    let mut buffer = [0u8; 1];
    match read(fd, &mut buffer) {
        Ok(0) => Ok(None),
        Ok(1) => Ok(Some(buffer[0])),
        Ok(_) => unreachable!(),
        Err(err) => Err(format!("read: failed to read from stdin ({err})").into()),
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

struct ReadResult {
    contents: ExpandedWord,
    reached_eof: bool,
    /// Bytes taken from a here-document, which has no file offset of its own
    /// and so must be shortened by the caller. Meaningless for real fds.
    consumed_bytes: usize,
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
                    result.append(bytes_to_string(std::mem::take(&mut buffer))?, false, true);
                    result.append(bytes_to_string(vec![next])?, true, true);
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
        std::thread::sleep(Duration::from_millis(16));
    }
    if !buffer.is_empty() {
        result.append(bytes_to_string(buffer)?, false, true);
    }
    Ok(ReadResult {
        contents: result,
        reached_eof: false,
        consumed_bytes: 0,
    })
}

fn read_until_from_file(
    fd: RawFd,
    delimiter: u8,
    backslash_escape: bool,
    continuation_prompt: Option<Prompt<'_>>,
) -> Result<ReadResult, BuiltinError> {
    let mut buffer = Vec::new();
    let mut result = ExpandedWord::default();
    let mut escape_next = false;
    let mut reached_eof = true;
    while let Some(next) = read_byte(fd)? {
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
                result.append(bytes_to_string(std::mem::take(&mut buffer))?, false, true);
                result.append(bytes_to_string(vec![next])?, true, true);
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
        result.append(bytes_to_string(buffer)?, false, true);
    }
    Ok(ReadResult {
        contents: result,
        reached_eof,
        consumed_bytes: 0,
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
            std::io::stdin().as_raw_fd(),
            delimiter,
            backslash_escape,
            continuation_prompt,
        )
    }
}

fn read_from_here_document(content: &str, delimiter: u8, backslash_escape: bool) -> ReadResult {
    let mut buffer = String::new();
    let mut result = ExpandedWord::default();
    let mut reached_eof = true;
    let mut escape_next = false;
    let mut consumed_bytes = content.len();
    for (offset, c) in content.char_indices() {
        if c == delimiter as char {
            reached_eof = false;
            consumed_bytes = offset + c.len_utf8();
            break;
        }
        if backslash_escape && c == '\\' {
            if escape_next {
                buffer.push('\\');
                escape_next = false;
            } else {
                escape_next = true
            }
            continue;
        }

        if escape_next {
            result.append(buffer, false, true);
            result.append(c.to_string(), true, true);
            buffer = String::new();
            escape_next = false;
        } else {
            buffer.push(c);
        }
    }
    if !buffer.is_empty() {
        result.append(buffer, false, true);
    }
    ReadResult {
        contents: result,
        reached_eof,
        consumed_bytes,
    }
}

fn read_until(
    shell: &mut Shell,
    file: &OpenedFile,
    delimiter: u8,
    backslash_escape: bool,
    opened_files: &OpenedFiles,
) -> Result<ReadResult, BuiltinError> {
    // PS2 is written for each line continuation `read` consumes (only possible
    // without -r), but only when the shell is interactive *and* the input is
    // actually the terminal -- prompting for text coming from a file would be
    // noise. It goes through the shell's stderr so redirection is honored.
    let prompt_ps2 = shell.is_interactive
        && backslash_escape
        && matches!(file, OpenedFile::Stdin)
        && std::io::stdin().is_terminal();
    let continuation_prompt = if prompt_ps2 {
        Some(shell.get_ps2())
    } else {
        None
    };
    let continuation_prompt = continuation_prompt.as_deref().map(|prompt| Prompt {
        prompt,
        opened_files,
    });
    match file {
        OpenedFile::Stdin => {
            read_from_stdin(shell, delimiter, backslash_escape, continuation_prompt)
        }
        OpenedFile::ReadFile(file) => read_until_from_file(
            file.as_raw_fd(),
            delimiter,
            backslash_escape,
            continuation_prompt,
        ),
        OpenedFile::ReadWriteFile(file) => read_until_from_file(
            file.as_raw_fd(),
            delimiter,
            backslash_escape,
            continuation_prompt,
        ),
        OpenedFile::HereDocument(contents) => {
            // A here-document has no file offset, so the text just read is
            // removed from the shared buffer; the next `read` continues after
            // it, as it would with a real descriptor.
            let result = read_from_here_document(&contents.borrow(), delimiter, backslash_escape);
            let mut remaining = contents.borrow_mut();
            let consumed = result.consumed_bytes.min(remaining.len());
            remaining.drain(..consumed);
            Ok(result)
        }
        _ => Err(gettext("read: invalid standard input").into()),
    }
}

pub struct BuiltinRead;

impl BuiltinUtility for BuiltinRead {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
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

        let stdin = opened_files
            .get_file(STDIN_FILENO)
            .cloned()
            .ok_or(BuiltinError::from("read: standard input is not opened"))?;
        let input = read_until(
            shell,
            &stdin,
            delim.unwrap_or(b'\n'),
            backslash_escape,
            opened_files,
        )?;

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
                .assign_global(vars[i].clone(), fields[i].to_string())
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
                if shell.assign_global(var.clone(), String::new()).is_err() {
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
