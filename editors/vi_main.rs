//! vi/ex - POSIX visual/line editor
//!
//! This is the entry point for both vi and ex binaries.
//! The mode is determined by argv[0]: if it ends with "ex",
//! the editor starts in ex (line) mode; otherwise in visual mode.

use std::env;
use std::process;
use vi_rs::{run_editor, InvokedAs};

fn main() {
    let args: Vec<String> = env::args().collect();
    let invoked_as = InvokedAs::detect();
    let exit_code = run_editor(invoked_as, &args);
    process::exit(exit_code);
}
