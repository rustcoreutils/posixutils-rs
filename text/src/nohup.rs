use gettextrs::{bind_textdomain_codeset, textdomain};
use nix::libc;
use nix::sys::signal::{self, SigHandler, Signal};
use nix::unistd::isatty;
use plib::PROJECT_NAME;
use std::env;
use std::fs::{File, OpenOptions};
use std::io;
use std::os::unix::io::AsRawFd;
use std::process::{self, Command};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;
    // Ignoring the SIGHUP signal
    unsafe {
        signal::signal(Signal::SIGHUP, SigHandler::SigIgn).expect("Failed to ignore SIGHUP");
    }

    // Save the original stderr
    let original_stderr = nix::unistd::dup(libc::STDERR_FILENO)?;

    // Getting the command and arguments
    let mut args = env::args().skip(1);
    let command = match args.next() {
        Some(cmd) => cmd,
        None => {
            eprintln!("Usage: nohup <command> [args...]");
            process::exit(127);
        }
    };

    // Redirecting stdout and stderr to the nohup.out file if they are connected to a terminal
    if isatty(libc::STDOUT_FILENO)? || isatty(libc::STDERR_FILENO)? {
        let nohup_out_file =
            get_nohup_out_file().expect("Failed to open nohup.out in current or home directory");

        if isatty(libc::STDOUT_FILENO)? {
            let fd = nohup_out_file.0.as_raw_fd();
            dup2(fd, libc::STDOUT_FILENO).expect("Failed to redirect stdout");
            match nohup_out_file.1 {
                NohupDir::Current => {
                    eprintln!(
                        "Name of the file to which the output is being appended: `nohup.out`"
                    );
                }
                NohupDir::Home => {
                    eprintln!(
                        "Name of the file to which the output is being appended: `$HOME/nohup.out`"
                    );
                }
            }
        }

        if isatty(libc::STDERR_FILENO)? {
            let fd = nohup_out_file.0.as_raw_fd();
            dup2(fd, libc::STDERR_FILENO).expect("Failed to redirect stderr");
        }
    }

    match Command::new(command).args(args).spawn() {
        Ok(mut process) => {
            process::exit(process.wait()?.code().unwrap_or(127));
        }
        Err(error) => {
            use std::io::ErrorKind;
            // Restore the original stderr
            dup2(original_stderr, libc::STDERR_FILENO).expect("Failed to restore stderr");
            // Close the duplicated descriptor as it's no longer needed
            nix::unistd::close(original_stderr)?;
            match error.kind() {
                ErrorKind::NotFound => {
                    eprintln!("Error: command not found");
                    process::exit(127);
                }
                _ => {
                    eprintln!("Error: command found but could not be invoked");
                    process::exit(126);
                }
            }
        }
    }
}

enum NohupDir {
    Current,
    Home,
}

fn get_nohup_out_file() -> Result<(File, NohupDir), io::Error> {
    // Attempting to open or create a nohup.out file in the current directory
    match OpenOptions::new()
        .create(true)
        .append(true)
        .open("nohup.out")
    {
        Ok(file) => Ok((file, NohupDir::Current)),
        Err(_) => {
            // If unsuccessful, attempt to create a nohup.out file in the home directory
            if let Some(home_dir) = dirs::home_dir() {
                let mut home_nohup_path = home_dir;
                home_nohup_path.push("nohup.out");
                let file = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(home_nohup_path)?;
                Ok((file, NohupDir::Home))
            } else {
                Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    "Home directory not found",
                ))
            }
        }
    }
}

fn dup2(old_fd: i32, new_fd: i32) -> Result<(), nix::Error> {
    if old_fd != new_fd {
        nix::unistd::dup2(old_fd, new_fd)?;
    }
    Ok(())
}
