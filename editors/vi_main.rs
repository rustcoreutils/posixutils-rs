//! vi - POSIX visual editor
//!
//! This is the entry point for the vi binary.
//! It delegates to the common editor core with visual mode.

use std::env;
use std::process;
use vi_rs::{run_editor, InvokedAs};

fn main() {
    let args: Vec<String> = env::args().collect();
    let exit_code = run_editor(InvokedAs::Vi, &args);
    process::exit(exit_code);
}
