//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;

#[derive(Debug, PartialEq, Eq)]
enum CdArgs<'a> {
    ChangeDir {
        directory: Option<&'a str>,
        handle_dot_dot_physically: bool,
    },
    GoBack,
}

impl<'a> CdArgs<'a> {
    fn parse(args: &'a [String]) -> Result<Self, String> {
        let mut handle_dot_dot_physically = false;

        let mut option_parser = OptionParser::new(args);

        while let Some(option) = option_parser
            .next_option()
            .map_err(|err| format!("cd: invalid option ({err})"))?
        {
            match option {
                'L' => {
                    handle_dot_dot_physically = false;
                }
                'P' => {
                    handle_dot_dot_physically = true;
                }
                _ => {
                    return Err(format!("cd: invalid option -{option}"));
                }
            }
        }

        let directory = args.get(option_parser.next_argument()).map(|s| s.as_str());
        if directory.is_some_and(|op| op == "-") {
            return Ok(Self::GoBack);
        }
        Ok(Self::ChangeDir {
            directory,
            handle_dot_dot_physically,
        })
    }
}

fn io_err_to_string<Err: Display>(err: Err) -> String {
    format!("cd: io error ({err})\n")
}

pub struct Cd;

impl BuiltinUtility for Cd {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = match CdArgs::parse(args) {
            Ok(args) => args,
            Err(err) => {
                return Err(format!("cd: {}\n", err).into());
            }
        };

        match args {
            CdArgs::ChangeDir {
                directory,
                handle_dot_dot_physically,
            } => {
                let dir = if let Some(dir) = directory {
                    dir
                } else if let Some(home_dir) = shell.environment.get_str_value("HOME") {
                    home_dir
                } else {
                    // behaviour is implementation defined, bash just returns 0
                    // and doesn't change directory
                    return Ok(0);
                };
                let mut curr_path = OsString::new();

                if !dir.starts_with('/') {
                    if !dir.starts_with('.') && !dir.starts_with("..") {
                        if let Some(cdpath) = shell.environment.get_str_value("CDPATH") {
                            for component in cdpath.split(':') {
                                let mut path = PathBuf::from(component);
                                path.push(dir);
                                if path.exists() {
                                    curr_path = path.into_os_string();
                                }
                            }
                        } else {
                            let path = PathBuf::from(dir);
                            if path.exists() {
                                curr_path = path.into_os_string();
                            }
                        }
                    }
                    if curr_path.is_empty() {
                        curr_path = OsString::from_vec(dir.as_bytes().to_vec());
                    }
                } else {
                    curr_path = OsString::from_vec(dir.as_bytes().to_vec());
                }

                if !handle_dot_dot_physically {
                    if !curr_path.as_bytes().first().is_some_and(|c| *c == b'/') {
                        let mut new_curr_path = shell
                            .environment
                            .get_str_value("PWD")
                            .unwrap_or_default()
                            .as_bytes()
                            .to_vec();
                        if new_curr_path.last().is_some_and(|c| *c != b'/') {
                            new_curr_path.push(b'/');
                        }
                        new_curr_path.extend(curr_path.as_bytes());
                        curr_path = OsString::from_vec(new_curr_path)
                    }
                    curr_path = PathBuf::from(curr_path)
                        .canonicalize()
                        .map_err(io_err_to_string)?
                        .into_os_string();
                }

                let old_working_dir = std::env::current_dir().map_err(io_err_to_string)?;
                nix::unistd::chdir(AsRef::<OsStr>::as_ref(&curr_path)).map_err(io_err_to_string)?;
                shell.current_directory = curr_path.clone();

                shell.assign_global("PWD".to_string(), curr_path.to_string_lossy().into_owned())?;
                shell.assign_global(
                    "OLDPWD".to_string(),
                    old_working_dir.to_string_lossy().into_owned(),
                )?;
            }
            CdArgs::GoBack => {
                if let Some(oldpwd) = shell
                    .environment
                    .get_str_value("OLDPWD")
                    .map(|s| s.to_string())
                {
                    let old_working_dir = std::env::current_dir().unwrap();
                    nix::unistd::chdir(oldpwd.as_str()).map_err(io_err_to_string)?;
                    shell.current_directory = OsString::from_vec(oldpwd.as_bytes().to_vec());
                    shell.assign_global("PWD".to_string(), oldpwd.clone())?;
                    shell.assign_global(
                        "OLDPWD".to_string(),
                        old_working_dir.to_string_lossy().into_owned(),
                    )?;
                    opened_files.write_out(format!("{}\n", oldpwd));
                    return Ok(0);
                }
                return Err("cd: OLDPWD not set\n".into());
            }
        }

        Ok(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_and_check_eq(args: Vec<&str>, correct: CdArgs<'static>) {
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let parsed_args = CdArgs::parse(&args).expect("invalid args");
        assert_eq!(parsed_args, correct);
    }

    #[test]
    fn parse_empty_args() {
        parse_and_check_eq(
            vec![],
            CdArgs::ChangeDir {
                directory: None,
                handle_dot_dot_physically: false,
            },
        )
    }

    #[test]
    fn parse_change_directory_no_args() {
        parse_and_check_eq(
            vec!["some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: false,
            },
        )
    }

    #[test]
    fn parse_change_directory_single_arg() {
        parse_and_check_eq(
            vec!["-L", "some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: false,
            },
        );
        parse_and_check_eq(
            vec!["-P", "some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: true,
            },
        )
    }

    #[test]
    fn parse_change_directory_multiple_args() {
        parse_and_check_eq(
            vec!["-L", "-P", "some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: true,
            },
        );
        parse_and_check_eq(
            vec!["-P", "-L", "some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: false,
            },
        );
        parse_and_check_eq(
            vec!["-PL", "some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: false,
            },
        );
        parse_and_check_eq(
            vec!["-PLPPLLP", "some_dir/some_other_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir/some_other_dir"),
                handle_dot_dot_physically: true,
            },
        )
    }

    #[test]
    fn parse_go_back_no_args() {
        parse_and_check_eq(vec!["-"], CdArgs::GoBack);
    }

    #[test]
    fn correctly_handle_options_terminator() {
        parse_and_check_eq(
            vec!["--"],
            CdArgs::ChangeDir {
                directory: None,
                handle_dot_dot_physically: false,
            },
        );
        parse_and_check_eq(
            vec!["--", "-L"],
            CdArgs::ChangeDir {
                directory: Some("-L"),
                handle_dot_dot_physically: false,
            },
        );
        parse_and_check_eq(
            vec!["--", "-L"],
            CdArgs::ChangeDir {
                directory: Some("-L"),
                handle_dot_dot_physically: false,
            },
        );
        parse_and_check_eq(vec!["--", "-"], CdArgs::GoBack);
        parse_and_check_eq(
            vec!["-P", "--", "some_dir"],
            CdArgs::ChangeDir {
                directory: Some("some_dir"),
                handle_dot_dot_physically: true,
            },
        );
    }
}
