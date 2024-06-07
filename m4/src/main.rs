use std::process::ExitCode;

use clap::Parser;
use m4::error::GetExitCode;

fn main() -> ExitCode {
    env_logger::init();
    let args = m4::Args::parse();

    let mut stdout = std::io::stdout();
    let mut stderr = std::io::stderr();
    if let Err(error) = m4::run(&mut stdout, &mut stderr, args) {
        ExitCode::from(u8::try_from(error.get_exit_code()).unwrap_or_else(|e| {
            eprintln!("Error casting exit code {e} into platform agnostic u8");
            1
        }))
    } else {
        ExitCode::SUCCESS
    }
}
