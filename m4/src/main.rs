use std::process::ExitCode;

use posixutils_m4::error::GetExitCode;

fn main() -> ExitCode {
    env_logger::init();
    let args = posixutils_m4::Args::parse();

    let stdout = std::io::stdout();
    let mut stderr = std::io::stderr();
    match posixutils_m4::run(stdout, &mut stderr, args) {
        Err(error) => ExitCode::from(u8::try_from(error.get_exit_code()).unwrap_or_else(|e| {
            eprintln!("Error casting exit code {e} into platform agnostic u8");
            1
        })),
        _ => ExitCode::SUCCESS,
    }
}
