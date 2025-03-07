use crate::builtin::BuiltinUtility;
use crate::shell::environment::Environment;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::PathBuf;

struct CdArgs<'a> {
    directory: Option<&'a str>,
    handle_dot_dot_physically: bool,
}

impl<'a> CdArgs<'a> {
    fn parse(args: &'a [String]) -> Result<Self, &'static str> {
        let mut handle_dot_dot_physically = false;
        for (i, arg) in args.iter().enumerate() {
            match arg.as_str() {
                "-L" => handle_dot_dot_physically = false,
                "-P" => handle_dot_dot_physically = true,
                directory => {
                    if i != args.len() - 1 {
                        return Err("too many arguments");
                    }
                    return Ok(Self {
                        directory: Some(directory),
                        handle_dot_dot_physically,
                    });
                }
            }
        }
        Ok(Self {
            directory: None,
            handle_dot_dot_physically: true,
        })
    }
}

pub struct Cd;

impl BuiltinUtility for Cd {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: OpenedFiles,
        environment: Environment,
    ) -> i32 {
        if args.len() == 1 && args[0] == "-" {
            if let Some(oldpwd) = environment.get_str_value("OLDPWD") {
                let old_working_dir = std::env::current_dir().unwrap();
                // TODO: handle errors
                nix::unistd::chdir(oldpwd).unwrap();
                shell.assign("PWD".to_string(), oldpwd.to_string(), false);
                shell.assign(
                    "OLDPWD".to_string(),
                    old_working_dir.to_string_lossy().into_owned(),
                    false,
                );
                opened_files.stdout().write_str(format!("{}\n", oldpwd));
                return 0;
            }
            opened_files.stderr().write_str("cd: OLDPWD not set");
            return 1;
        }

        let args = match CdArgs::parse(args) {
            Ok(args) => args,
            Err(err) => {
                opened_files.stderr().write_str(format!("sh: {}", err));
                return 2;
            }
        };

        let dir = if let Some(dir) = args.directory {
            dir
        } else {
            if let Some(home_dir) = environment.get_str_value("HOME") {
                home_dir
            } else {
                // behaviour is implementation defined, bash just returns 0
                // and doesn't change directory
                return 0;
            }
        };
        let mut curr_path = OsString::new();

        if !dir.starts_with('/') {
            if !dir.starts_with('.') && !dir.starts_with("..") {
                if let Some(cdpath) = environment.get_str_value("CDPATH") {
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

        if !args.handle_dot_dot_physically {
            if !curr_path.as_bytes().get(0).is_some_and(|c| *c == b'/') {
                let mut new_curr_path = environment
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
            // TODO: handle error
            curr_path = PathBuf::from(curr_path)
                .canonicalize()
                .unwrap()
                .into_os_string();
        }

        // TODO: handle errors
        let old_working_dir = std::env::current_dir().unwrap();
        nix::unistd::chdir(AsRef::<OsStr>::as_ref(&curr_path)).unwrap();

        shell.assign(
            "PWD".to_string(),
            curr_path.to_string_lossy().into_owned(),
            false,
        );
        shell.assign(
            "OLDPWD".to_string(),
            old_working_dir.to_string_lossy().into_owned(),
            false,
        );

        0
    }
}
