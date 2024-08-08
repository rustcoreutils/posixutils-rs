#[cfg(target_os = "macos")]
mod psmacos;

#[cfg(target_os = "linux")]
mod pslinux;

use clap::Parser;

#[cfg(target_os = "macos")]
mod platform {
    pub use crate::psmacos::*;
}

#[cfg(target_os = "linux")]
mod platform {
    pub use crate::pslinux::*;
}

#[derive(Parser)]
#[command(name = "ps")]
#[command(about = "Report process status", version = "1.0")]
struct Args {
    /// List all processes
    #[arg(short = 'A', long)]
    all: bool,
}

fn main() {
    let args = Args::parse();

    if args.all {
        match platform::list_processes() {
            Ok(processes) => {
                println!(
                    "{:<5} {:<5} {:<5} {:<5} {}",
                    "PID", "PPID", "UID", "GID", "COMMAND"
                );
                for proc in processes {
                    println!(
                        "{:<5} {:<5} {:<5} {:<5} {}",
                        proc.pid, proc.ppid, proc.uid, proc.gid, proc.path
                    );
                }
            }
            Err(e) => eprintln!("Error: {}", e),
        }
    }
}
