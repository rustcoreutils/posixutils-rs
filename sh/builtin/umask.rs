//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

fn format_mode(mode: u32) -> String {
    let types = ["", "x", "w", "wx", "r", "rx", "rw", "rwx"];
    let user = types[((mode >> 6) & 7) as usize];
    let group = types[((mode >> 3) & 7) as usize];
    let others = types[(mode & 7) as usize];

    format!("u={},g={},o={}\n", user, group, others)
}

/// Applies a chmod-style symbolic mode (e.g. `u=rwx,go=rx`, `a-w`) to the
/// current *allowed-permission* bits (`shell.umask` stores the complement of
/// the mask, so the symbolic operations apply to it directly).
fn apply_symbolic(current: u32, spec: &str) -> Result<u32, String> {
    let invalid = || format!("umask: invalid mask '{spec}'");
    let mut mode = current;
    for clause in spec.split(',') {
        let bytes = clause.as_bytes();
        let mut i = 0;
        let mut who_mask = 0u32;
        while i < bytes.len() && matches!(bytes[i], b'u' | b'g' | b'o' | b'a') {
            match bytes[i] {
                b'u' => who_mask |= 0o700,
                b'g' => who_mask |= 0o070,
                b'o' => who_mask |= 0o007,
                b'a' => who_mask |= 0o777,
                _ => unreachable!(),
            }
            i += 1;
        }
        if who_mask == 0 {
            who_mask = 0o777; // an omitted "who" means all
        }
        let op = *bytes.get(i).ok_or_else(invalid)?;
        if !matches!(op, b'+' | b'-' | b'=') {
            return Err(invalid());
        }
        i += 1;
        let mut perm = 0u32;
        while i < bytes.len() {
            match bytes[i] {
                b'r' => perm |= 4,
                b'w' => perm |= 2,
                b'x' => perm |= 1,
                _ => return Err(invalid()),
            }
            i += 1;
        }
        let mut perm_bits = 0u32;
        if who_mask & 0o700 != 0 {
            perm_bits |= perm << 6;
        }
        if who_mask & 0o070 != 0 {
            perm_bits |= perm << 3;
        }
        if who_mask & 0o007 != 0 {
            perm_bits |= perm;
        }
        match op {
            b'=' => mode = (mode & !who_mask) | perm_bits,
            b'+' => mode |= perm_bits,
            b'-' => mode &= !perm_bits,
            _ => unreachable!(),
        }
    }
    Ok(mode)
}

pub struct Umask;

impl BuiltinUtility for Umask {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let mut option_parser = OptionParser::new(args);

        let mut produce_symbolic_output = false;
        while let Some(option) = option_parser
            .next_option()
            .map_err(|opt| format!("umask: invalid option -{opt}"))?
        {
            match option {
                'S' => {
                    produce_symbolic_output = true;
                }
                opt => {
                    return Err(format!("umask: invalid option -{opt}").into());
                }
            }
        }

        if option_parser.next_argument() == args.len() {
            if produce_symbolic_output {
                opened_files.write_out(format_mode(shell.umask));
            } else {
                opened_files.write_out(format!("{:04o}\n", !shell.umask & 0o777));
            }
        } else if option_parser.next_argument() == args.len() - 1 {
            let mask_arg = &args[option_parser.next_argument()];
            if mask_arg.starts_with(|c: char| c.is_ascii_digit()) {
                let new_umask = u32::from_str_radix(mask_arg, 8)
                    .map_err(|_| format!("umask: invalid mask '{mask_arg}'"))?;
                if new_umask > 0o777 {
                    return Err(format!("umask: invalid mask '{mask_arg}'").into());
                }
                shell.umask = !new_umask & 0o777;
            } else {
                // symbolic mode operates on the allowed-permission bits
                shell.umask = apply_symbolic(shell.umask, mask_arg)?;
            }
        } else {
            return Err("umask: too many arguments".into());
        }

        Ok(0)
    }
}
