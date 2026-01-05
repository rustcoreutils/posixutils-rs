//! ex - POSIX line editor
//!
//! This is the entry point for the ex binary.
//! It delegates to the common editor core with ex (line) mode.

use std::env;
use std::process;
use vi_rs::{InvokedAs, run_editor};

fn main() {
    let args: Vec<String> = env::args().collect();
    let exit_code = run_editor(InvokedAs::Ex, &args);
    process::exit(exit_code);
}
