//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{skip_option_terminator, BuiltinResult, SpecialBuiltinUtility};
use crate::parse::command_parser::is_valid_name;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::shstr::ShString;
use gettextrs::gettext;

pub struct Export;

impl SpecialBuiltinUtility for Export {
    fn exec(
        &self,
        args: &[ShString],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        // `-p` is POSIX. `-n` (take the export attribute away) is a
        // bash/ksh extension, forced by binutils, whose top-level Makefile
        // runs `export -n ELF_PACKAGE_METADATA` under /bin/sh. As in bash,
        // the two may be combined (`-pn`) and, with no operand, either one
        // lists the exported variables.
        let mut print = false;
        let mut unexport = false;
        let mut option_count = 0;
        for arg in args {
            let Some(letters) = arg.strip_prefix(b"-") else {
                break;
            };
            if letters.is_empty() || !letters.iter().all(|&b| b == b'n' || b == b'p') {
                break;
            }
            print |= letters.contains(&b'p');
            unexport |= letters.contains(&b'n');
            option_count += 1;
        }
        let args = skip_option_terminator(&args[option_count..]);

        if print && !unexport && !args.is_empty() {
            return Err(gettext("export: too many arguments").into());
        }
        if (print || unexport) && args.is_empty() {
            let mut pairs = shell
                .environment
                .global_scope()
                .iter()
                .filter(|(_, val)| val.export)
                .collect::<Vec<_>>();
            pairs.sort_by_key(|(k, _)| k.as_str());
            for (var, var_value) in pairs {
                if let Some(val) = &var_value.value {
                    // Built as bytes: this output is meant to be read back by
                    // the shell, so a value that is not text must survive it.
                    let mut line = ShString::from(format!("export {}=", var));
                    line.push_bytes(crate::utils::shell_quote(val));
                    line.push_bytes(b"\n");
                    opened_files.write_out(line);
                } else {
                    opened_files.write_out(format!("export {}\n", var));
                }
            }
            return Ok(0);
        }

        if args.is_empty() {
            return Err(gettext("export: too few arguments").into());
        }

        for arg in args {
            // A *name* is restricted to the portable character set, but the
            // value it is being given is an arbitrary byte string.
            let (name, value) = if let Some(pos) = arg.iter().position(|&b| b == b'=') {
                let name = std::str::from_utf8(&arg[..pos]).unwrap_or("");
                if !is_valid_name(name) {
                    return Err(format!(
                        "export: '{}' is not a valid name",
                        crate::shstr::ShStr::new(&arg[..pos]).display()
                    )
                    .into());
                }
                (
                    name.to_string(),
                    Some(ShString::from(arg[pos + 1..].to_vec())),
                )
            } else {
                let name = arg.to_str().unwrap_or("");
                if !is_valid_name(name) {
                    return Err(format!("export: '{}' is not a valid name\n", arg.display()).into());
                }
                (name.to_string(), None)
            };
            if let Some(value) = value {
                shell.assign_global(name, value)?.export = !unexport;
            } else if !unexport {
                shell.environment.promote_local_or_get_global(name).export = true;
            } else if shell.environment.get_value(&name).is_some()
                || shell.environment.global_scope().contains_key(&name)
            {
                // A name never mentioned is left alone. A readonly variable
                // may lose the attribute: that changes only what children
                // see, not the value, as in bash.
                shell.environment.promote_local_or_get_global(name).export = false;
            }
        }
        Ok(0)
    }
}
