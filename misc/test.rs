//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The `test` / `[` utility. The expression evaluator lives in
//! `plib::test_expr`, shared with the shell's built-in version.

use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::test_expr::{eval_posix_strict, EvalResult};
use std::ffi::OsStr;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // setlocale establishes libc's global locale, which strcoll(3) (used by the
    // `<` and `>` operators) consults, so LC_COLLATE takes effect. (audit #1)
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    // Operands are byte strings: a path or a compared string need not be text.
    let mut args: Vec<Vec<u8>> = std::env::args_os().map(|a| a.into_vec()).collect();

    // Detect if invoked as "[" or "test"
    let prog_name = Path::new(OsStr::from_bytes(&args[0]))
        .file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_default();
    let is_bracket = prog_name == "[";

    // If program name is "[", then final arg must be "]"
    if is_bracket {
        if args.last().map(Vec::as_slice) != Some(b"]".as_slice()) {
            eprintln!("{}", gettext("missing closing bracket"));
            std::process::exit(2);
        }
        args.pop();
    }

    // Remove program name
    args.remove(0);

    let result = eval_posix_strict(&args);

    match result {
        EvalResult::True => std::process::exit(0),
        EvalResult::False => std::process::exit(1),
        EvalResult::Error(msg) => {
            eprintln!("test: {}", msg);
            std::process::exit(2);
        }
    }
}
