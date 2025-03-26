//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinError, BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::{OpenedFile, OpenedFiles, STDIN_FILENO};
use crate::shell::Shell;
use crate::wordexp::expanded_word::ExpandedWord;
use crate::wordexp::split_fields;
use atty::Stream;
use nix::errno::Errno;
use std::os::fd::{AsRawFd, RawFd};
use std::time::Duration;

fn bytes_to_string(bytes: Vec<u8>) -> Result<String, BuiltinError> {
    String::from_utf8(bytes.to_vec()).map_err(|_| "read: invalid UTF-8".into())
}

fn read_byte_non_blocking(fd: RawFd) -> Result<Option<u8>, BuiltinError> {
    let mut buffer = [0u8; 1];
    match nix::unistd::read(fd, &mut buffer) {
        Ok(0) => Ok(None),
        Ok(1) => Ok(Some(buffer[0])),
        Ok(_) => unreachable!(),
        Err(Errno::EAGAIN) => Ok(None),
        Err(err) => Err(format!("read: failed to read from stdin ({err})").into()),
    }
}

fn read_byte(fd: RawFd) -> Result<Option<u8>, BuiltinError> {
    let mut buffer = [0u8; 1];
    match nix::unistd::read(fd, &mut buffer) {
        Ok(0) => Ok(None),
        Ok(1) => Ok(Some(buffer[0])),
        Ok(_) => unreachable!(),
        Err(err) => Err(format!("read: failed to read from stdin ({err})").into()),
    }
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
) -> Result<ReadResult, BuiltinError> {
    let mut buffer = Vec::new();
    let mut result = ExpandedWord::default();
    let mut escape_next = false;
    'outer: loop {
        while let Some(next) = read_byte_non_blocking(fd)? {
            if next == delimiter {
                break 'outer;
            }
            if backslash_escape && next == b'\\' {
                if escape_next {
                    buffer.push(b'\\');
                    escape_next = false;
                } else {
                    escape_next = true;
                }
                continue;
            }
            if escape_next {
                result.append(bytes_to_string(buffer)?, false, true);
                result.append(bytes_to_string(vec![next])?, true, true);
                buffer = Vec::new();
                escape_next = false;
            } else {
                buffer.push(next);
            }
        }
        // might receive signals while reading
        shell.update_global_state();
        std::thread::sleep(Duration::from_millis(16));
    }
    if !buffer.is_empty() {
        result.append(bytes_to_string(buffer)?, false, true);
    }
    Ok(ReadResult {
        contents: result,
        reached_eof: false,
    })
}

fn read_until_from_file(
    fd: RawFd,
    delimiter: u8,
    backslash_escape: bool,
) -> Result<ReadResult, BuiltinError> {
    let mut buffer = Vec::new();
    let mut result = ExpandedWord::default();
    let mut escape_next = false;
    let mut reached_eof = true;
    while let Some(next) = read_byte(fd)? {
        if next == delimiter {
            reached_eof = false;
            break;
        }
        if backslash_escape && next == b'\\' {
            if escape_next {
                buffer.push(b'\\');
                escape_next = false;
            } else {
                escape_next = true;
            }
            continue;
        }
        if escape_next {
            result.append(bytes_to_string(buffer)?, false, true);
            result.append(bytes_to_string(vec![next])?, true, true);
            buffer = Vec::new();
            escape_next = false;
        } else {
            buffer.push(next);
        }
    }

    if !buffer.is_empty() {
        result.append(bytes_to_string(buffer)?, false, true);
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
) -> Result<ReadResult, BuiltinError> {
    if atty::is(Stream::Stdin) {
        let original_terminal_settings = shell.terminal.reset();
        shell.terminal.set_nonblocking();

        let result = read_until_from_non_blocking_fd(
            shell,
            std::io::stdin().as_raw_fd(),
            delimiter,
            backslash_escape,
        );

        shell.terminal.set(original_terminal_settings);

        result
    } else {
        read_until_from_file(std::io::stdin().as_raw_fd(), delimiter, backslash_escape)
    }
}

fn read_from_here_document(content: &str, delimiter: u8, backslash_escape: bool) -> ReadResult {
    let mut buffer = String::new();
    let mut result = ExpandedWord::default();
    let mut reached_eof = true;
    let mut escape_next = false;
    for c in content.chars() {
        if c == delimiter as char {
            reached_eof = false;
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
    }
}

fn read_until(
    shell: &mut Shell,
    file: &OpenedFile,
    delimiter: u8,
    backslash_escape: bool,
) -> Result<ReadResult, BuiltinError> {
    match file {
        OpenedFile::Stdin => read_from_stdin(shell, delimiter, backslash_escape),
        OpenedFile::ReadFile(file) => {
            read_until_from_file(file.as_raw_fd(), delimiter, backslash_escape)
        }
        OpenedFile::ReadWriteFile(file) => {
            read_until_from_file(file.as_raw_fd(), delimiter, backslash_escape)
        }
        OpenedFile::HereDocument(contents) => Ok(read_from_here_document(
            contents,
            delimiter,
            backslash_escape,
        )),
        _ => Err("read: invalid standard input".into()),
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
                        return Err("read: -d can only be specified once".into());
                    }
                    let arg = option_parser
                        .next_option_argument()
                        .ok_or("read: -d requires an argument")?;
                    if arg.len() > 1 {
                        return Err("read: -d requires a single character".into());
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
            return Err("read: missing operand".into());
        }
        let vars = &args[first_operand..];

        let stdin = opened_files
            .get_file(STDIN_FILENO)
            .ok_or(BuiltinError::from("read: standard input is not opened"))?;
        let input = read_until(shell, stdin, delim.unwrap_or(b'\n'), backslash_escape)?;

        let fields = split_fields(
            input.contents,
            shell.environment.get_str_value("IFS"),
            args.len(),
        );

        for i in 0..fields.len() {
            shell
                .assign_global(vars[i].clone(), fields[i].to_string())
                .map_err(|_| format!("read: cannot set readonly variable {}", vars[i]))?;
        }
        if fields.len() < vars.len() {
            for var in &vars[fields.len()..] {
                shell
                    .assign_global(var.clone(), String::new())
                    .map_err(|_| format!("read: cannot set readonly variable {}", var))?;
            }
        }

        if input.reached_eof {
            Ok(1)
        } else {
            Ok(0)
        }
    }
}
