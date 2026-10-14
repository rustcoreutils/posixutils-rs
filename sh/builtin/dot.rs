//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::os::{dup_cloexec, find_command};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::{execute_script_source, Shell};
use crate::shstr::{ShStr, ShString};
use gettextrs::gettext;
use plib::diag::io_error_text;
use std::fs::File;
use std::io::Read;
use std::os::fd::RawFd;

pub struct Dot;

/// The descriptor that `/dev/stdin` or `/dev/fd/N` names, if `path` is one.
fn descriptor_named_by(path: &ShStr) -> Option<u32> {
    let path = path.as_bytes();
    if path == b"/dev/stdin" {
        return Some(0);
    }
    let number = path.strip_prefix(b"/dev/fd/")?;
    std::str::from_utf8(number).ok()?.parse().ok()
}

/// Reads the script from descriptor `fd` of the shell, the one the path
/// names, through a copy: reading consumes the input, as reading the
/// descriptor itself would, and works for an unnamed file such as a
/// here-document's, which reopening the path may not on every system.
fn read_descriptor(fd: u32) -> Result<Vec<u8>, String> {
    let copy = RawFd::try_from(fd)
        .ok()
        .and_then(|raw| dup_cloexec(raw).ok())
        .ok_or_else(|| format!("dot: /dev/fd/{fd}: {}\n", gettext("cannot open file")))?;
    let mut source = Vec::new();
    File::from(copy)
        .read_to_end(&mut source)
        .map_err(|err| read_failure(&format!("/dev/fd/{fd}"), &err))?;
    Ok(source)
}

/// The diagnostic for a script `dot` opened but could not read.
fn read_failure(name: &str, err: &std::io::Error) -> String {
    format!(
        "dot: {} '{name}': {}",
        gettext("cannot read"),
        io_error_text(err)
    )
}

/// Finds the file named by `name` along PATH (XCU dot) and reads it.
fn read_script_file(name: &ShStr, shell: &Shell) -> Result<Vec<u8>, String> {
    let path = shell.environment.get_str_value("PATH").unwrap_or_default();
    // `find_command` is shared with command lookup, where executability
    // rather than readability is the gate; the dot utility reads the file,
    // it does not execute it.
    let Some(file_path) = find_command(name, path) else {
        return Err(format!(
            "dot: {}, no such file or directory\n",
            name.display()
        ));
    };
    let mut file = File::open(&file_path).map_err(|_| {
        format!(
            "dot: {}: {}\n",
            file_path.to_string_lossy(),
            gettext("cannot open file")
        )
    })?;
    let mut source = Vec::new();
    file.read_to_end(&mut source)
        .map_err(|err| read_failure(&file_path.to_string_lossy(), &err))?;
    Ok(source)
}

impl SpecialBuiltinUtility for Dot {
    fn exec(&self, args: &[ShString], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
        let args = skip_option_terminator(args);

        if args.len() != 1 {
            return Err(gettext("dot: incorrect number of arguments").into());
        }

        let source = match descriptor_named_by(&args[0]) {
            Some(fd) => read_descriptor(fd)?,
            None => read_script_file(&args[0], shell)?,
        };

        shell.dot_script_depth += 1;
        let result = execute_script_source(shell, &source);
        shell.dot_script_depth -= 1;

        result.map_err(|parser_err| {
            format!(
                "dot: parsing error ({}): {}\n",
                parser_err.lineno, parser_err.message
            )
            .into()
        })
    }
}
