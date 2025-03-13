use crate::builtin::BuiltinUtility;
use crate::shell::environment::Environment;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use std::ffi::{OsStr, OsString};
use std::fmt::Display;
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

fn io_err_to_string<Err: Display>(err: Err) -> String {
    format!("cd: io error ({err})\n")
}

fn cd(
    args: &[String],
    shell: &mut Shell,
    opened_files: &OpenedFiles,
    environment: Environment,
) -> Result<(), String> {
    if args.len() == 1 && args[0] == "-" {
        if let Some(oldpwd) = environment.get_str_value("OLDPWD") {
            let old_working_dir = std::env::current_dir().unwrap();
            nix::unistd::chdir(oldpwd).map_err(io_err_to_string)?;
            shell.current_directory = OsString::from_vec(oldpwd.as_bytes().to_vec());
            shell.assign("PWD".to_string(), Some(oldpwd.to_string()), false, false);
            shell.assign(
                "OLDPWD".to_string(),
                Some(old_working_dir.to_string_lossy().into_owned()),
                false,
                false,
            );
            opened_files.stdout().write_str(format!("{}\n", oldpwd));
            return Ok(());
        }
        return Err("cd: OLDPWD not set\n".to_string());
    }

    let args = match CdArgs::parse(args) {
        Ok(args) => args,
        Err(err) => {
            return Err(format!("cd: {}\n", err));
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
            return Ok(());
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
        curr_path = PathBuf::from(curr_path)
            .canonicalize()
            .map_err(io_err_to_string)?
            .into_os_string();
    }

    let old_working_dir = std::env::current_dir().map_err(io_err_to_string)?;
    nix::unistd::chdir(AsRef::<OsStr>::as_ref(&curr_path)).map_err(io_err_to_string)?;
    shell.current_directory = curr_path.clone();

    shell.assign(
        "PWD".to_string(),
        Some(curr_path.to_string_lossy().into_owned()),
        false,
        false,
    );
    shell.assign(
        "OLDPWD".to_string(),
        Some(old_working_dir.to_string_lossy().into_owned()),
        false,
        false,
    );

    Ok(())
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
        match cd(args, shell, &opened_files, environment) {
            Ok(_) => 0,
            Err(message) => {
                opened_files.stderr().write_str(message);
                1
            }
        }
    }
}
