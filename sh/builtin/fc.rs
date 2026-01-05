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
use crate::os::{mkstemp, write};
use crate::shell::Shell;
use crate::shell::history::EndPoint;
use crate::shell::opened_files::OpenedFiles;
use std::fs::File;
use std::io::Read;
use std::os::fd::FromRawFd;
use std::path::PathBuf;

#[derive(Debug, Eq, PartialEq)]
struct Replace<'s> {
    old: &'s str,
    new: &'s str,
}

#[derive(Debug, Eq, PartialEq)]
enum Action {
    List,
    Edit,
    Reexecute,

    None,
}

#[derive(Debug, Eq, PartialEq)]
enum FcArgs<'s> {
    Edit {
        editor: Option<&'s str>,
        reverse: bool,
        first: Option<EndPoint<'s>>,
        last: Option<EndPoint<'s>>,
    },
    List {
        reverse: bool,
        suppress_command_number: bool,
        first: Option<EndPoint<'s>>,
        last: Option<EndPoint<'s>>,
    },
    Reexecute {
        replace: Option<Replace<'s>>,
        first: Option<EndPoint<'s>>,
    },
}

impl<'s> FcArgs<'s> {
    fn parse(args: &'s [String]) -> Result<Self, String> {
        let mut editor = None;
        let mut reverse = false;
        let mut suppress_command_number = false;
        let mut action = Action::None;

        let mut option_parser = OptionParser::new(args);

        while let Some(option) = option_parser
            .next_option()
            .map_err(|opt| format!("fc: invalid option {opt}"))?
        {
            match option {
                'r' => {
                    if action == Action::Reexecute {
                        return Err("fc: cannot use -r and -s".to_string());
                    }
                    reverse = true;
                }
                'e' => {
                    if action != Action::None {
                        return Err("fc: cannot use -e with -l or -s".to_string());
                    }
                    action = Action::Edit;
                    editor = option_parser.next_option_argument();
                }
                'l' => {
                    if action != Action::None && action != Action::List {
                        return Err("fc: cannot use -l with -e or -s".to_string());
                    }
                    action = Action::List;
                }
                'n' => {
                    if action != Action::List {
                        return Err("fc: -n can only be specified after -l".to_string());
                    }
                    suppress_command_number = true;
                }
                's' => {
                    if action != Action::None {
                        return Err("fc: cannot use -s with -e or -l".to_string());
                    }
                    action = Action::Reexecute;
                }
                other => return Err(format!("fc: invalid option -{other}")),
            }
        }

        let first_operand = option_parser.next_argument();

        if args.len() - first_operand > 2 {
            return Err("fc: too many operands".to_string());
        }

        if action == Action::Reexecute {
            let mut replace = None;
            let mut first = None;
            if let Some(first_op) = args.get(first_operand) {
                if let Some((old, new)) = first_op.split_once('=') {
                    replace = Some(Replace { old, new });
                } else {
                    first = Some(EndPoint::parse(first_op));
                }
                if first.is_none() {
                    if let Some(last_op) = args.get(first_operand + 1) {
                        first = Some(EndPoint::parse(last_op));
                    }
                } else if args.len() - first_operand > 1 {
                    return Err("fc: too many operands".to_string());
                }
            }
            Ok(FcArgs::Reexecute { first, replace })
        } else {
            let mut first = None;
            let mut last = None;

            if let Some(first_op) = args.get(first_operand) {
                first = Some(EndPoint::parse(first_op));
                if let Some(last_op) = args.get(first_operand + 1) {
                    last = Some(EndPoint::parse(last_op));
                }
            }
            if action == Action::List {
                Ok(FcArgs::List {
                    reverse,
                    suppress_command_number,
                    first,
                    last,
                })
            } else {
                Ok(FcArgs::Edit {
                    editor,
                    reverse,
                    first,
                    last,
                })
            }
        }
    }
}

fn execute_history(
    history: &str,
    shell: &mut Shell,
    opened_files: &mut OpenedFiles,
    keep_in_history: bool,
) -> Result<(), BuiltinError> {
    std::mem::swap(opened_files, &mut shell.opened_files);
    let result = shell.execute_program(history);
    std::mem::swap(opened_files, &mut shell.opened_files);
    if !keep_in_history {
        shell.history.remove_last_entry();
    }
    result
        .map(|_| ())
        .map_err(|err| format!("fc: syntax error: {}", err.message).into())
}

fn open_editor_with_file(
    editor: &str,
    file_path: PathBuf,
    shell: &mut Shell,
    opened_files: &OpenedFiles,
) -> Result<i32, BuiltinError> {
    let command_path = shell
        .find_command(editor, "", shell.set_options.hashall)
        .ok_or("fc: editor not found")?;
    let args = vec![editor.to_string(), file_path.to_string_lossy().to_string()];
    Ok(shell.fork_and_exec(command_path, &args, opened_files)?)
}

fn edit(
    shell: &mut Shell,
    reverse: bool,
    editor: Option<&str>,
    first: Option<EndPoint>,
    last: Option<EndPoint>,
    opened_files: &mut OpenedFiles,
) -> Result<(), BuiltinError> {
    let history = match (first, last) {
        (None, None) => shell
            .history
            .list(EndPoint::Last(1), EndPoint::Last(1), reverse, false)
            .map_err(|err| format!("fc: {err}"))?,
        (Some(first), None) => shell
            .history
            .list(first, EndPoint::Last(1), reverse, false)
            .map_err(|err| format!("fc: {err}"))?,
        (Some(first), Some(last)) => shell
            .history
            .list(first, last, reverse, false)
            .map_err(|err| format!("fc: {err}"))?,
        _ => unreachable!("cannot have last without first"),
    };
    // remove call to fc
    shell.history.remove_last_entry();
    let (fd, path) = mkstemp("/tmp/sh-fc.XXXXXX")
        .map_err(|err| format!("fc: failed to create temporary file: {err}"))?;
    write(fd, history.as_bytes())
        .map_err(|err| format!("fc: failed to write to temporary file ({err})"))?;
    let editor = editor
        .unwrap_or_else(|| shell.environment.get_str_value("FCEDIT").unwrap_or("ed"))
        .to_string();
    let result = open_editor_with_file(&editor, path, shell, opened_files)?;
    if result != 0 {
        return Ok(());
    }
    let mut file = unsafe { File::from_raw_fd(fd) };
    let mut edited_contents = String::new();
    file.read_to_string(&mut edited_contents)
        .map_err(|err| format!("fc: failed to read edited commands ({err})"))?;
    // if the command wasn't edited, we don't add it to the history again
    execute_history(
        &edited_contents,
        shell,
        opened_files,
        edited_contents != history,
    )?;
    Ok(())
}

fn list(
    shell: &mut Shell,
    opened_files: &mut OpenedFiles,
    reverse: bool,
    suppress_command_number: bool,
    first: Option<EndPoint>,
    last: Option<EndPoint>,
) -> Result<(), BuiltinError> {
    let history = match (first, last) {
        (None, None) => shell
            .history
            .list(
                EndPoint::Last(16),
                EndPoint::Last(1),
                reverse,
                !suppress_command_number,
            )
            .map_err(|err| format!("fc: {err}"))?,
        (Some(first), None) => shell
            .history
            .list(first, EndPoint::Last(1), reverse, !suppress_command_number)
            .map_err(|err| format!("fc: {err}"))?,
        (Some(first), Some(last)) => shell
            .history
            .list(first, last, reverse, !suppress_command_number)
            .map_err(|err| format!("fc: {err}"))?,
        _ => unreachable!("cannot have last without first"),
    };
    opened_files.write_out(history);
    Ok(())
}

pub struct Fc;

impl BuiltinUtility for Fc {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = FcArgs::parse(args)?;

        match args {
            FcArgs::Edit {
                reverse,
                editor,
                first,
                last,
            } => {
                edit(shell, reverse, editor, first, last, opened_files)?;
            }
            FcArgs::List {
                reverse,
                suppress_command_number,
                first,
                last,
            } => {
                list(
                    shell,
                    opened_files,
                    reverse,
                    suppress_command_number,
                    first,
                    last,
                )?;
            }
            FcArgs::Reexecute { replace, first } => {
                let first = first.unwrap_or(EndPoint::Last(1));
                let history = shell
                    .history
                    .list(first, EndPoint::Last(1), false, false)
                    .map_err(|err| format!("fc: {err}"))?;
                // this call to fc should not be in the history
                shell.history.remove_last_entry();
                if let Some(replacement) = replace {
                    let history = history.replace(replacement.old, replacement.new);
                    execute_history(&history, shell, opened_files, true)?;
                } else {
                    // command was not changed and is already in the history, so we don't add it.
                    // The standard doesn't specify this, but its what bash does, and it seems
                    // like a good idea
                    execute_history(&history, shell, opened_files, false)?;
                }
            }
        }

        Ok(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::option_parser::tests::to_args;

    #[test]
    fn parse_no_args() {
        let args = vec![];
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Edit {
                reverse: false,
                editor: None,
                first: None,
                last: None,
            })
        );
    }

    #[test]
    fn parse_edit_with_editor() {
        let args = to_args(vec!["-e", "vim"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Edit {
                reverse: false,
                editor: Some("vim"),
                first: None,
                last: None,
            })
        );
    }

    #[test]
    fn parse_edit_with_first() {
        let args = to_args(vec!["-e", "vim", "1"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Edit {
                reverse: false,
                editor: Some("vim"),
                first: Some(EndPoint::CommandNumber(1)),
                last: None
            })
        );
    }

    #[test]
    fn parse_edit_with_first_and_last() {
        let args = to_args(vec!["-e", "vim", "1", "command"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Edit {
                reverse: false,
                editor: Some("vim"),
                first: Some(EndPoint::CommandNumber(1)),
                last: Some(EndPoint::String("command"))
            })
        );
    }

    #[test]
    fn parse_reexecute_no_args() {
        let args = to_args(vec!["-s"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Reexecute {
                first: None,
                replace: None
            })
        );
    }

    #[test]
    fn parse_reexecute_with_replace() {
        let args = to_args(vec!["-s", "old=new"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Reexecute {
                first: None,
                replace: Some(Replace {
                    old: "old",
                    new: "new"
                })
            })
        );
    }

    #[test]
    fn parse_reexecute_with_first() {
        let args = to_args(vec!["-s", "1"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Reexecute {
                first: Some(EndPoint::CommandNumber(1)),
                replace: None
            })
        );
    }

    #[test]
    fn parse_reexecute_with_replace_and_first() {
        let args = to_args(vec!["-s", "old=new", "1"]);
        assert_eq!(
            FcArgs::parse(&args),
            Ok(FcArgs::Reexecute {
                replace: Some(Replace {
                    old: "old",
                    new: "new"
                }),
                first: Some(EndPoint::CommandNumber(1)),
            })
        );
    }
}
