//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use std::error::Error;
use std::fs;
use std::io::Write;
use std::io::{stderr, stdout, ErrorKind};
use std::path::PathBuf;
use std::path::{Component, Path};

/// readlink — display the contents of a symbolic link
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Do not output a trailing <newline> character.
    #[arg(short, long)]
    no_newline: bool,

    // Not POSIX, but implemented by BusyBox, FreeBSD, GNU Core Utilities, toybox, and others
    /// Canonicalize the provided path, resolving symbolic links repeatedly if needed. The absolute path of the resolved file is printed.
    #[arg(short = 'f')]
    canonicalize: bool,

    /// Print an error description to standard error when an error occurs and the specified file could not be resolved
    #[arg(short = 'v')]
    verbose: bool,

    /// The pathname of an existing symbolic link
    pathname: PathBuf,
}

// Behavior of "readlink -f /existent-directory/non-existent-file" varies
// Most implementations: print hypothetical fully resolved absolute path, exit code 0
// bsdutils/FreeBSD, toybox: print nothing, exit code 1
//
// Behavior of "readlink -f /non-existent-directory/non-existent-file" does not vary
// All implementations: print nothing, exit code 1
fn do_readlink(args: Args) -> Result<String, String> {
    let Args {
        no_newline,
        canonicalize,
        verbose,
        pathname,
    } = args;

    let pathname_path = pathname.as_path();

    let format_error = |description: &str, error: Option<&dyn Error>| {
        let pathname_path_display = pathname_path.display();

        let st = if let Some(er) = error {
            format!("{pathname_path_display}: {description}: {er}")
        } else {
            format!("{pathname_path_display}: {description}")
        };

        Result::<String, String>::Err(st)
    };

    let format_returned_path = |path_to_return: &Path| {
        let path_to_return_display = path_to_return.display();

        let st = if no_newline {
            format!("{path_to_return_display}")
        } else {
            format!("{path_to_return_display}\n")
        };

        Result::<String, String>::Ok(st)
    };

    let map_io_error = |error: &std::io::Error| {
        match error.kind() {
            ErrorKind::NotFound => {
                // All or almost all other implementations do not print an error here
                // (but they do exit with exit code 1)
                if verbose {
                    format_error("No such file or directory", None)
                } else {
                    Err(String::new())
                }
            }
            ErrorKind::PermissionDenied => {
                if verbose {
                    format_error("Permission denied", None)
                } else {
                    Err(String::new())
                }
            }
            _ => format_error("Unknown error", Some(&error)),
        }
    };

    if canonicalize {
        let recursively_resolved_path_buf = recursive_resolve(pathname_path.to_owned())?;

        match fs::canonicalize(recursively_resolved_path_buf.as_path()) {
            Ok(pa) => format_returned_path(pa.as_path()),
            Err(er) => {
                let mut components = recursively_resolved_path_buf.components();

                // Check if the last component of the path is a "normal" component
                // (e.g. "normal-component" in "/prefix/normal-component/suffix")
                //
                // If so, the fallback path (since the path itself could not be canonicalized)
                // is to canonicalize the parent directory path, and append the last path component
                if let Some(Component::Normal(last_component)) = components.next_back() {
                    let parent_path = components.as_path();

                    match fs::canonicalize(parent_path) {
                        Ok(parent_path_canonicalized) => {
                            // Before printing the hypothetical resolved path:
                            // ensure that the parent is actually a directory
                            if !parent_path_canonicalized.is_dir() {
                                return format_error("Not a directory", None);
                            }

                            let parent_path_canonicalized_with_last_component = {
                                let mut pa = parent_path_canonicalized;

                                pa.push(last_component);

                                pa
                            };

                            format_returned_path(
                                parent_path_canonicalized_with_last_component.as_path(),
                            )
                        }
                        Err(err) => map_io_error(&err),
                    }
                } else {
                    map_io_error(&er)
                }
            }
        }
    } else {
        match fs::symlink_metadata(pathname_path) {
            Ok(me) => {
                if !me.is_symlink() {
                    // POSIX says:
                    // "If file does not name a symbolic link, readlink shall write a diagnostic message to standard error and exit with non-zero status."
                    // However, this is violated by almost all implementations
                    return if verbose {
                        format_error("Not a symbolic link", None)
                    } else {
                        Err(String::new())
                    };
                }

                match fs::read_link(pathname_path) {
                    Ok(pa) => format_returned_path(pa.as_path()),
                    Err(er) => map_io_error(&er),
                }
            }
            Err(er) => map_io_error(&er),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let exit_code = match do_readlink(args) {
        Ok(output) => {
            let mut stdout_lock = stdout().lock();

            write!(stdout_lock, "{output}").unwrap();

            stdout_lock.flush().unwrap();

            0_i32
        }
        Err(error_description) => {
            if !error_description.is_empty() {
                let mut stderr_lock = stderr().lock();

                writeln!(&mut stderr_lock, "readlink: {error_description}").unwrap();

                stderr_lock.flush().unwrap();
            }

            1_i32
        }
    };

    std::process::exit(exit_code);
}

fn recursive_resolve(starting_path_buf: PathBuf) -> Result<PathBuf, String> {
    let mut current_path_buf = starting_path_buf;

    let mut recursion_level = 0_usize;

    #[allow(clippy::while_let_loop)]
    loop {
        match fs::read_link(current_path_buf.as_path()) {
            Ok(pa) => {
                recursion_level += 1_usize;

                // https://unix.stackexchange.com/questions/53087/how-do-you-increase-maxsymlinks
                if recursion_level == 40_usize {
                    return Err(format!(
                        "Symbolic link chain is circular or just too long, gave up at \"{}\"",
                        current_path_buf.to_string_lossy()
                    ));
                }

                if pa.is_absolute() {
                    current_path_buf = pa;
                } else {
                    if !current_path_buf.pop() {
                        return Err(format!(
                            "Could not remove last path segment from path \"{}\"",
                            current_path_buf.to_string_lossy()
                        ));
                    }

                    current_path_buf.push(pa);
                }
            }
            Err(_) => {
                break;
            }
        }
    }

    Ok(current_path_buf)
}
