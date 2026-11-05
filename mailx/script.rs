//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command scripts: start-up files, `source`, and `if`/`else`/`endif`.

use crate::commands::{execute_in, Context, Interp};
use crate::error::Result;
use crate::mailbox::Mailbox;
use crate::variables::Variables;

/// One open `if`: whether its current branch runs, and whether that branch
/// is the `else`.
#[derive(Debug, Clone, Copy)]
struct Branch {
    runs: bool,
    in_else: bool,
}

/// The `if`/`else`/`endif` blocks enclosing the current line.
#[derive(Debug, Default)]
pub struct CondStack {
    open: Vec<Branch>,
}

impl CondStack {
    /// Whether commands run here: every enclosing branch must.
    pub fn active(&self) -> bool {
        self.open.iter().all(|b| b.runs)
    }

    fn push_if(&mut self, runs: bool) {
        self.open.push(Branch {
            runs,
            in_else: false,
        });
    }

    fn enter_else(&mut self) -> Result<()> {
        match self.open.pop() {
            Some(b) if !b.in_else => {
                self.open.push(Branch {
                    runs: !b.runs,
                    in_else: true,
                });
                Ok(())
            }
            _ => Err("unexpected else".into()),
        }
    }

    fn endif(&mut self) -> Result<()> {
        self.open
            .pop()
            .map(|_| ())
            .ok_or_else(|| "unexpected endif".into())
    }
}

/// Handle `if`/`else`/`endif`, returning `None` when `cmd` is none of them.
///
/// `if s` holds while mailx is in Send Mode, `if r` in Receive Mode; the
/// command loop is Receive Mode by definition.
pub fn conditional(cmd: &str, args: &str, interp: &mut Interp) -> Option<Result<()>> {
    let is = |name: &str, min: usize| {
        cmd.len() >= min && name.len() >= cmd.len() && name.starts_with(cmd)
    };

    if is("if", 1) {
        let runs = match args.trim() {
            "r" => !interp.send_mode,
            "s" => interp.send_mode,
            _ => false,
        };
        interp.cond.push_if(runs);
        Some(Ok(()))
    } else if is("else", 2) {
        Some(interp.cond.enter_else())
    } else if is("endif", 2) {
        Some(interp.cond.endif())
    } else {
        None
    }
}

/// Run the lines of `content` as a start-up script.
///
/// An error is reported and ends the script, the rest of the file unread --
/// one of the two behaviors POSIX allows, and the one BSD mailx has. An
/// unterminated `if` is diagnosed and its blocks closed, so a malformed file
/// cannot leave the rest of the session suppressed.
pub fn run_script(
    content: &str,
    path: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    interp: &mut Interp,
) {
    let depth = interp.cond.open.len();
    for line in content.lines() {
        if let Err(e) = execute_in(line, mb, vars, interp, Context::Startup) {
            eprintln!("mailx: {}: {}", path, e);
            break;
        }
    }
    if interp.cond.open.len() > depth {
        eprintln!("mailx: {}: missing endif", path);
        interp.cond.open.truncate(depth);
    }
}
