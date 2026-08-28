//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Command interpreter for mailx Receive Mode

use std::env;
use std::fs;
use std::io::{self, IsTerminal, Write};
use std::process::Command;

use crate::escapes::handle_escape;
use crate::mailbox::Mailbox;
use crate::message::{author_filename, Disposition, ReadState};
use crate::msglist::{msglist_or_current, parse_message, parse_msglist};
use crate::send::{compose_reply, send_message, ComposedMessage};
use crate::util::expand_local_prefixes;
use crate::variables::{parse_set_arg, split_args, Variables};

/// Result of executing a command
pub enum CommandResult {
    Continue,
    Quit,
    Exit,
}

/// Where a command line came from, which decides what is legal there.
///
/// The three contexts used to be three separate interpreters -- command mode,
/// start-up files, and `~:` from input mode -- each with its own abbreviation
/// list, its own `!` handling, and its own idea of which commands existed. They
/// drifted, so they are one interpreter and one table now, with the differences
/// expressed as a mask on each command.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Context {
    /// The interactive command loop.
    Command,
    /// A start-up file or a file read by `source`.
    Startup,
    /// A `~:` or `~_` request from input mode.
    Input,
}

impl Context {
    fn bit(self) -> u8 {
        match self {
            Context::Command => CMD,
            Context::Startup => RC,
            Context::Input => INPUT,
        }
    }

    fn where_invalid(self) -> &'static str {
        match self {
            Context::Command => "command not valid here",
            Context::Startup => "command not valid in a start-up file",
            Context::Input => "command not valid in input mode",
        }
    }
}

const CMD: u8 = 1 << 0;
const RC: u8 = 1 << 1;
const INPUT: u8 = 1 << 2;

/// Legal everywhere: needs no message store and changes only settings.
const ANY: u8 = CMD | RC | INPUT;
/// Legal in command mode and start-up files, but not from input mode.
const CMD_RC: u8 = CMD | RC;

type Handler = fn(&str, &mut Mailbox, &mut Variables) -> Result<CommandResult, String>;

/// One command: its full name, the shortest accepted abbreviation, where it is
/// legal, and what runs it.
struct Cmd {
    /// The full command name, in the case POSIX spells it.
    name: &'static str,
    /// Length of the shortest accepted prefix (the part before `[` in the
    /// POSIX synopsis).
    min: usize,
    /// Contexts this command may be used in.
    ctx: u8,
    run: Handler,
}

/// Every command, with the abbreviation rule POSIX states for it.
///
/// Matching is case-sensitive, which is the whole point: `Copy`, `Save`,
/// `Print`, `Type`, `Reply`, and `Followup` are distinct commands from their
/// lowercase forms (spec 104734-104742, 104949-104961, 104876, 104893-104902,
/// 104803-104813). Dispatching on a lowercased command word made all six
/// unreachable, and their handlers dead code.
///
/// The start-up exclusions are spec 104557-104559 exactly: `!`, `edit`, `hold`,
/// `mail`, `preserve`, `reply`, `Reply`, `Save`, `shell`, `visual`, `Copy`,
/// `followup`, and `Followup`. Note that lowercase `copy` and `save` are
/// *legal* there; only the capitalized forms are not.
const COMMANDS: &[Cmd] = &[
    Cmd {
        name: "alias",
        min: 1,
        ctx: ANY,
        run: |a, _, v| cmd_alias(a, v),
    },
    Cmd {
        name: "group",
        min: 1,
        ctx: ANY,
        run: |a, _, v| cmd_alias(a, v),
    },
    Cmd {
        name: "alternates",
        min: 3,
        ctx: ANY,
        run: |a, _, v| cmd_alternates(a, v),
    },
    Cmd {
        name: "echo",
        min: 2,
        ctx: ANY,
        run: |a, _, _| {
            println!("{}", a);
            Ok(CommandResult::Continue)
        },
    },
    Cmd {
        name: "set",
        min: 2,
        ctx: ANY,
        run: |a, _, v| cmd_set(a, v),
    },
    Cmd {
        name: "unset",
        min: 3,
        ctx: ANY,
        run: |a, _, v| cmd_unset(a, v),
    },
    Cmd {
        name: "cd",
        min: 2,
        ctx: CMD_RC,
        run: |a, _, _| cmd_cd(a),
    },
    Cmd {
        name: "chdir",
        min: 2,
        ctx: CMD_RC,
        run: |a, _, _| cmd_cd(a),
    },
    Cmd {
        name: "copy",
        min: 1,
        ctx: CMD_RC,
        run: |a, m, v| cmd_copy(a, m, v, false),
    },
    Cmd {
        name: "Copy",
        min: 1,
        ctx: CMD,
        run: |a, m, v| cmd_copy_author(a, m, v, false),
    },
    Cmd {
        name: "delete",
        min: 1,
        ctx: CMD_RC,
        run: cmd_delete,
    },
    Cmd {
        name: "discard",
        min: 2,
        ctx: CMD_RC,
        run: |a, _, v| cmd_discard(a, v),
    },
    Cmd {
        name: "ignore",
        min: 2,
        ctx: CMD_RC,
        run: |a, _, v| cmd_discard(a, v),
    },
    Cmd {
        name: "dp",
        min: 2,
        ctx: CMD_RC,
        run: cmd_dp,
    },
    Cmd {
        name: "dt",
        min: 2,
        ctx: CMD_RC,
        run: cmd_dp,
    },
    Cmd {
        name: "edit",
        min: 1,
        ctx: CMD,
        run: cmd_edit,
    },
    Cmd {
        name: "exit",
        min: 2,
        ctx: CMD_RC,
        run: |_, _, _| Ok(CommandResult::Exit),
    },
    Cmd {
        name: "xit",
        min: 1,
        ctx: CMD_RC,
        run: |_, _, _| Ok(CommandResult::Exit),
    },
    Cmd {
        name: "file",
        min: 2,
        ctx: CMD_RC,
        run: cmd_file,
    },
    Cmd {
        name: "folder",
        min: 4,
        ctx: CMD_RC,
        run: cmd_file,
    },
    Cmd {
        name: "folders",
        min: 7,
        ctx: CMD_RC,
        run: |_, _, v| cmd_folders(v),
    },
    Cmd {
        name: "followup",
        min: 2,
        ctx: CMD,
        run: |a, m, v| cmd_followup(a, m, v, false),
    },
    Cmd {
        name: "Followup",
        min: 2,
        ctx: CMD,
        run: |a, m, v| cmd_followup(a, m, v, true),
    },
    Cmd {
        name: "from",
        min: 1,
        ctx: CMD_RC,
        run: cmd_from,
    },
    Cmd {
        name: "headers",
        min: 1,
        ctx: CMD_RC,
        run: cmd_headers,
    },
    Cmd {
        name: "help",
        min: 3,
        ctx: CMD_RC,
        run: |_, _, _| cmd_help(),
    },
    Cmd {
        name: "hold",
        min: 2,
        ctx: CMD,
        run: cmd_hold,
    },
    Cmd {
        name: "preserve",
        min: 3,
        ctx: CMD,
        run: cmd_hold,
    },
    Cmd {
        name: "list",
        min: 1,
        ctx: CMD_RC,
        run: |_, _, _| cmd_list(),
    },
    Cmd {
        name: "mail",
        min: 1,
        ctx: CMD,
        run: cmd_mail,
    },
    Cmd {
        name: "mbox",
        min: 2,
        ctx: CMD_RC,
        run: cmd_mbox,
    },
    Cmd {
        name: "next",
        min: 1,
        ctx: CMD_RC,
        run: cmd_next,
    },
    Cmd {
        name: "pipe",
        min: 2,
        ctx: CMD_RC,
        run: cmd_pipe,
    },
    Cmd {
        name: "print",
        min: 1,
        ctx: CMD_RC,
        run: |a, m, v| cmd_print(a, m, v, false),
    },
    Cmd {
        name: "Print",
        min: 1,
        ctx: CMD_RC,
        run: |a, m, v| cmd_print(a, m, v, true),
    },
    Cmd {
        name: "type",
        min: 1,
        ctx: CMD_RC,
        run: |a, m, v| cmd_print(a, m, v, false),
    },
    Cmd {
        name: "Type",
        min: 1,
        ctx: CMD_RC,
        run: |a, m, v| cmd_print(a, m, v, true),
    },
    Cmd {
        name: "quit",
        min: 1,
        ctx: CMD_RC,
        run: |_, _, _| Ok(CommandResult::Quit),
    },
    Cmd {
        name: "reply",
        min: 1,
        ctx: CMD,
        run: |a, m, v| cmd_reply_flipr(a, m, v, true),
    },
    Cmd {
        name: "respond",
        min: 3,
        ctx: CMD,
        run: |a, m, v| cmd_reply_flipr(a, m, v, true),
    },
    Cmd {
        name: "Reply",
        min: 1,
        ctx: CMD,
        run: |a, m, v| cmd_reply_flipr(a, m, v, false),
    },
    Cmd {
        name: "Respond",
        min: 3,
        ctx: CMD,
        run: |a, m, v| cmd_reply_flipr(a, m, v, false),
    },
    Cmd {
        name: "retain",
        min: 3,
        ctx: CMD_RC,
        run: |a, _, v| cmd_retain(a, v),
    },
    Cmd {
        name: "save",
        min: 1,
        ctx: CMD_RC,
        run: |a, m, v| cmd_save(a, m, v, true),
    },
    Cmd {
        name: "Save",
        min: 1,
        ctx: CMD,
        run: |a, m, v| cmd_copy_author(a, m, v, true),
    },
    Cmd {
        name: "shell",
        min: 2,
        ctx: CMD,
        run: |_, _, v| cmd_shell_interactive(v),
    },
    Cmd {
        name: "size",
        min: 2,
        ctx: CMD_RC,
        run: cmd_size,
    },
    Cmd {
        name: "source",
        min: 2,
        ctx: CMD_RC,
        run: cmd_source,
    },
    Cmd {
        name: "top",
        min: 2,
        ctx: CMD_RC,
        run: cmd_top,
    },
    Cmd {
        name: "touch",
        min: 3,
        ctx: CMD_RC,
        run: cmd_touch,
    },
    Cmd {
        name: "unalias",
        min: 3,
        ctx: CMD_RC,
        run: |a, _, v| cmd_unalias(a, v),
    },
    Cmd {
        name: "undelete",
        min: 1,
        ctx: CMD_RC,
        run: cmd_undelete,
    },
    Cmd {
        name: "visual",
        min: 1,
        ctx: CMD,
        run: cmd_visual,
    },
    Cmd {
        name: "write",
        min: 1,
        ctx: CMD_RC,
        run: cmd_write,
    },
];

/// Commands spelled with punctuation rather than a name.
const PUNCTUATION: &[(&str, u8, Handler)] =
    &[("?", CMD_RC, |_, _, _| cmd_help()), ("|", CMD_RC, cmd_pipe)];

/// Find the command `word` names, if any.
///
/// A command may be entered as any prefix of its name at least `min` characters
/// long (spec 104689-104693). The comparison is case-sensitive.
fn lookup(word: &str, ctx: Context) -> Option<Result<&'static Cmd, String>> {
    let cmd = COMMANDS
        .iter()
        .find(|c| word.len() >= c.min && c.name.len() >= word.len() && c.name.starts_with(word))?;
    Some(if cmd.ctx & ctx.bit() == 0 {
        Err(format!("{}: {}", word, ctx.where_invalid()))
    } else {
        Ok(cmd)
    })
}

/// `reply`/`Reply` differ only in who the reply goes to, and `flipr` swaps
/// which spelling means which (spec 104899-104902).
fn cmd_reply_flipr(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    lowercase_form: bool,
) -> Result<CommandResult, String> {
    let reply_to_all = lowercase_form != vars.get_bool("flipr");
    cmd_reply(args, mb, vars, reply_to_all)
}

/// Execute a command in Receive Mode
pub fn execute_command(
    line: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
) -> Result<CommandResult, String> {
    execute_in(line, mb, vars, Context::Command)
}

/// Execute one command line in `ctx`.
///
/// This is the single interpreter. `mb` is a scratch, empty mailbox in the
/// start-up and input-mode contexts, where there is no message store; commands
/// that need one are excluded by their context mask rather than by a separate
/// dispatch table.
pub fn execute_in(
    line: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    ctx: Context,
) -> Result<CommandResult, String> {
    let line = line.trim();

    if line.is_empty() || line.starts_with('#') {
        return Ok(CommandResult::Continue);
    }

    let (cmd, args) = parse_command_line(line);

    // Conditionals are handled before the context check, so an `if` guarding a
    // command that is illegal here still suppresses it rather than diagnosing
    // it.
    if let Some(result) = conditional(cmd, args, vars) {
        return result.map(|_| CommandResult::Continue);
    }
    if !vars.cond_active() {
        return Ok(CommandResult::Continue);
    }

    // Shell escape. Not valid in a start-up file (spec 104557-104559).
    if let Some(rest) = line.strip_prefix('!') {
        if ctx == Context::Startup {
            return Err("!: command not valid in a start-up file".to_string());
        }
        return cmd_shell(rest, vars);
    }

    // Current message number.
    if line == "=" {
        println!("{}", mb.current);
        return Ok(CommandResult::Continue);
    }

    if let Some((_, bits, run)) = PUNCTUATION.iter().find(|(name, _, _)| *name == cmd) {
        if bits & ctx.bit() == 0 {
            return Err(format!("{}: {}", cmd, ctx.where_invalid()));
        }
        return run(args, mb, vars);
    }

    // Scrolling: `z`, `z+`, `z-`.
    if let Some(direction) = cmd.strip_prefix('z') {
        if direction.is_empty() || direction == "+" || direction == "-" {
            if ctx != Context::Command {
                return Err(format!("{}: {}", cmd, ctx.where_invalid()));
            }
            return cmd_scroll(direction, mb, vars);
        }
    }

    if let Some(found) = lookup(cmd, ctx) {
        return (found?.run)(args, mb, vars);
    }

    // A bare message number selects and prints that message.
    if let Ok(num) = cmd.parse::<usize>() {
        if ctx != Context::Command {
            return Err(format!("{}: {}", cmd, ctx.where_invalid()));
        }
        return if num > 0 && num <= mb.message_count() {
            mb.current = num;
            cmd_print("", mb, vars, false)
        } else {
            Err(format!("Invalid message number: {}", num))
        };
    }

    Err(format!("Unknown command: {}", cmd))
}

/// Handle `if`/`else`/`endif`, returning `None` when `cmd` is none of them.
///
/// One engine for both contexts. There used to be two, disagreeing on whether a
/// stray `else` was a warning or an error, and on whether `if s` was even
/// recognized; a third copy in the start-up dispatcher matched the same words
/// and did nothing with them.
fn conditional(cmd: &str, args: &str, vars: &mut Variables) -> Option<Result<(), String>> {
    let is = |name: &str, min: usize| {
        cmd.len() >= min && name.len() >= cmd.len() && name.starts_with(cmd)
    };

    if is("if", 1) {
        // `s` is true while reading a start-up file for Send Mode, `r` for
        // Receive Mode. The command loop is Receive Mode by definition.
        let matches = match args.trim() {
            "r" => !vars.send_mode,
            "s" => vars.send_mode,
            _ => false,
        };
        vars.cond_stack.push((matches, false));
        return Some(Ok(()));
    }
    if is("else", 2) {
        return Some(match vars.cond_stack.pop() {
            Some((_, true)) | None => Err("unexpected else".to_string()),
            Some((matches, false)) => {
                vars.cond_stack.push((!matches, true));
                Ok(())
            }
        });
    }
    if is("endif", 2) {
        return Some(if vars.cond_stack.pop().is_none() {
            Err("unexpected endif".to_string())
        } else {
            Ok(())
        });
    }
    None
}

fn parse_command_line(line: &str) -> (&str, &str) {
    let line = line.trim();
    if let Some(pos) = line.find(char::is_whitespace) {
        (&line[..pos], line[pos..].trim())
    } else {
        (line, "")
    }
}

// ============ Command implementations ============

/// Report what a `save`, `copy`, `Save`, `Copy`, or `write` wrote.
fn report_written(mb: &Mailbox, msg_nums: &[usize], filename: &str) {
    let total: usize = msg_nums
        .iter()
        .filter_map(|&n| mb.get(n))
        .map(|m| m.size())
        .sum();
    println!(
        "\"{}\" {} messages {} bytes",
        filename,
        msg_nums.len(),
        total
    );
}

/// Mark messages saved and note that the mailbox needs writing back.
fn mark_messages_saved(mb: &mut Mailbox, msg_nums: &[usize]) {
    for &num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.disposition = Disposition::Saved;
        }
    }
    mb.modified = true;
}

/// Mark messages read and make the last of them current.
fn mark_read(mb: &mut Mailbox, msg_nums: &[usize]) {
    for &num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.read = ReadState::Read;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.current = last;
    }
}

/// Split `[msglist] file` into its two halves.
///
/// The trailing word is the filename; everything before it is the message list.
/// With a single argument the intent is ambiguous, so a msglist that parses
/// wins and anything else is taken as a filename.
fn msglist_and_file(
    args: &str,
    mb: &Mailbox,
    vars: &Variables,
    file_required: bool,
) -> Result<(Vec<usize>, String), String> {
    let parts: Vec<&str> = args.split_whitespace().collect();
    match parts.len() {
        0 => {
            if file_required {
                return Err("No file specified".to_string());
            }
            Ok((vec![mb.current], crate::util::mbox_path(vars)))
        }
        1 => match parse_msglist(parts[0], mb, false, vars) {
            Ok(nums) if !file_required => Ok((nums, crate::util::mbox_path(vars))),
            _ => Ok((vec![mb.current], expand_filename(parts[0], vars))),
        },
        n => {
            let nums = parse_msglist(&parts[..n - 1].join(" "), mb, false, vars)?;
            Ok((nums, expand_filename(parts[n - 1], vars)))
        }
    }
}

pub(crate) fn cmd_alias(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        // Print all aliases
        let mut names: Vec<&String> = vars.aliases.keys().collect();
        names.sort();
        for name in names {
            if let Some(addrs) = vars.aliases.get(name) {
                println!("{}\t{}", name, addrs.join(" "));
            }
        }
    } else {
        let parts: Vec<&str> = args.split_whitespace().collect();
        if parts.len() == 1 {
            // Print specific alias
            if let Some(addrs) = vars.aliases.get(parts[0]) {
                println!("{}\t{}", parts[0], addrs.join(" "));
            }
        } else {
            // Set alias
            let name = parts[0].to_string();
            let addrs: Vec<String> = parts[1..].iter().map(|s| s.to_string()).collect();
            vars.aliases.insert(name, addrs);
        }
    }
    Ok(CommandResult::Continue)
}

pub(crate) fn cmd_alternates(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        println!("{}", vars.alternates.join(" "));
    } else {
        for name in args.split_whitespace() {
            if !vars.alternates.contains(&name.to_string()) {
                vars.alternates.push(name.to_string());
            }
        }
    }
    Ok(CommandResult::Continue)
}

fn cmd_cd(args: &str) -> Result<CommandResult, String> {
    let dir = if args.is_empty() {
        env::var("HOME").unwrap_or_else(|_| ".".to_string())
    } else {
        args.to_string()
    };

    env::set_current_dir(&dir).map_err(|e| format!("{}: {}", dir, e))?;
    Ok(CommandResult::Continue)
}

fn cmd_copy(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    mark_saved: bool,
) -> Result<CommandResult, String> {
    let (msg_nums, filename) = msglist_and_file(args, mb, vars, true)?;

    mb.save_messages(&msg_nums, &filename, true)?;

    if mark_saved {
        mark_messages_saved(mb, &msg_nums);
    }

    report_written(mb, &msg_nums, &filename);

    Ok(CommandResult::Continue)
}

/// `Copy` and `Save`: file the messages under the author's name.
///
/// The two differ only in whether the messages are then marked saved
/// (spec 104738-104742, 104958-104961), so they are one function. They were two
/// near-identical copies, and -- because dispatch lowercased the command word
/// before matching -- both were unreachable.
fn cmd_copy_author(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    mark_saved: bool,
) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    if let Some(first_msg) = msg_nums.first().and_then(|&n| mb.get(n)) {
        let filename = author_filename(first_msg.from())?;
        let filename = expand_author_filename(filename, vars);

        mb.save_messages(&msg_nums, &filename, true)?;

        if mark_saved {
            mark_messages_saved(mb, &msg_nums);
        }

        report_written(mb, &msg_nums, &filename);
    }

    Ok(CommandResult::Continue)
}

fn cmd_delete(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let max_deleted = msg_nums.iter().copied().max().unwrap_or(0);

    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.disposition = Disposition::Deleted;
        }
    }

    mb.modified = true;

    // Update current message
    if let Some(next) = mb.next_undeleted(max_deleted) {
        mb.current = next;
    } else if let Some(prev) = mb.prev_undeleted(max_deleted) {
        mb.current = prev;
    }

    // If autoprint is set, print the new current message
    if vars.get_bool("autoprint")
        && mb.current > 0
        && mb
            .get(mb.current)
            .map(|m| m.disposition != Disposition::Deleted)
            .unwrap_or(false)
    {
        cmd_print("", mb, vars, false)?;
    }

    Ok(CommandResult::Continue)
}

fn cmd_discard(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        for h in &vars.ignored_headers {
            println!("{}", h);
        }
    } else {
        for header in args.split_whitespace() {
            if !vars.ignored_headers.contains(&header.to_string()) {
                vars.ignored_headers.push(header.to_string());
            }
        }
    }
    Ok(CommandResult::Continue)
}

fn cmd_dp(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    // Delete and print next
    cmd_delete(args, mb, vars)?;

    // Print the new current message
    if mb.current > 0
        && mb
            .get(mb.current)
            .map(|m| m.disposition != Disposition::Deleted)
            .unwrap_or(false)
    {
        cmd_print("", mb, vars, false)?;
    } else {
        println!("At EOF");
    }

    Ok(CommandResult::Continue)
}

fn cmd_edit(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let editor = vars.get("EDITOR").unwrap_or("ed");
    edit_messages(&msg_nums, mb, editor)
}

/// Run `editor` over each message in turn, on a private temporary copy.
///
/// The temporary comes from `mkstemp` (`O_EXCL`, mode 0600, honoring `$TMPDIR`)
/// rather than a name built from the pid: `/tmp/mailx.<pid>.<n>` is predictable,
/// world-readable, and `fs::write` follows a symlink planted there first.
fn edit_messages(msg_nums: &[usize], mb: &Mailbox, editor: &str) -> Result<CommandResult, String> {
    for &num in msg_nums {
        if let Some(msg) = mb.get(num) {
            let mut tmp = plib::tmp::NamedTempFile::new().map_err(|e| e.to_string())?;
            tmp.as_file_mut()
                .write_all(msg.format_full().as_bytes())
                .map_err(|e| e.to_string())?;
            tmp.as_file_mut().flush().map_err(|e| e.to_string())?;

            Command::new(editor)
                .arg(tmp.path())
                .status()
                .map_err(|e| e.to_string())?;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_file(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        // Print current mailbox info
        println!("\"{}\" {} messages", mb.path, mb.undeleted_count());
        return Ok(CommandResult::Continue);
    }

    // Resolve the target, handling `#` (the previously-opened folder).
    let path = if args.trim() == "#" {
        match &mb.prev_path {
            Some(p) => p.clone(),
            None => return Err("No previous folder".to_string()),
        }
    } else {
        expand_filename(args, vars)
    };

    // Remember the folder we are leaving for a later `#`.
    let leaving = mb.path.clone();

    // Check the new folder can be opened *before* flushing the old one, so a
    // bad path does not leave the session pointing at a mailbox already written
    // out. The load itself has to happen after the flush: reading it first
    // would record a byte length taken before `quit` rewrote the file, and if
    // the two are the same mailbox everything `quit` appended would then look
    // like newly delivered mail and be copied through a second time.
    fs::File::open(&path).map_err(|e| format!("{}: {}", path, e))?;
    mb.quit(vars)?;
    let mut new_mb = Mailbox::load(&path).map_err(|e| format!("{}: {}", path, e))?;
    new_mb.prev_path = Some(leaving);

    *mb = new_mb;
    mb.print_headers(None, vars);

    Ok(CommandResult::Continue)
}

fn cmd_folders(vars: &mut Variables) -> Result<CommandResult, String> {
    if let Some(folder) = vars.get("folder") {
        let folder = expand_filename(folder, vars);
        let lister = vars.get("LISTER").unwrap_or("ls");
        crate::util::shell(&format!("{} {}", lister, folder), vars)?;
    } else {
        println!("No folder variable set");
    }
    Ok(CommandResult::Continue)
}

fn cmd_followup(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    reply_to_all: bool,
) -> Result<CommandResult, String> {
    let msg_num = if args.is_empty() {
        mb.current
    } else {
        parse_message(args, mb, vars)?
    };

    let original = mb.get(msg_num).ok_or("No message")?;
    let mut composed = compose_reply(original, reply_to_all, vars);

    // Record to file named after first recipient
    let record_file = author_filename(original.from())?;
    let record_file = expand_author_filename(record_file, vars);

    // Enter input mode
    compose_body(&mut composed, Some(mb), vars)?;

    // Send and record
    send_message(&composed, vars, false)?;
    // `followup` records through the same writer as `record`, so it honors
    // `outfolder` -- its private copy of this logic did not.
    crate::send::record_message(&composed, &record_file, vars)?;

    if let Some(m) = mb.get_mut(msg_num) {
        m.read = ReadState::Read;
    }

    Ok(CommandResult::Continue)
}

fn cmd_from(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    mb.print_headers(Some(&msg_nums), vars);
    Ok(CommandResult::Continue)
}

fn cmd_headers(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
) -> Result<CommandResult, String> {
    if !args.is_empty() {
        let msg_num = parse_message(args, mb, vars)?;
        mb.current = msg_num;
    }

    let screen = vars.screen_lines().get();
    let start = if mb.current > 0 {
        ((mb.current - 1) / screen) * screen + 1
    } else {
        1
    };

    let nums: Vec<usize> = (start..=mb.message_count().min(start + screen - 1)).collect();
    mb.print_headers(Some(&nums), vars);

    Ok(CommandResult::Continue)
}

/// The command list, generated from the dispatch table.
///
/// `help` and `list` used to carry hand-written inventories. They had already
/// drifted from what dispatch accepted: `list` advertised `Copy`, `Save`,
/// `Print`, `Type`, `Reply`, and `Followup`, none of which could run.
fn command_names() -> Vec<&'static str> {
    let mut names: Vec<&'static str> = COMMANDS.iter().map(|c| c.name).collect();
    names.extend(PUNCTUATION.iter().map(|(n, _, _)| *n));
    names.push("z");
    names
}

fn cmd_help() -> Result<CommandResult, String> {
    println!("Commands:");
    for chunk in command_names().chunks(6) {
        println!("  {}", chunk.join("  "));
    }
    println!("Any command may be abbreviated to its shortest unique prefix.");
    Ok(CommandResult::Continue)
}

fn cmd_hold(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    // Allowed only in the system mailbox (spec 104831).
    if !mb.is_system_mailbox {
        return Err("hold: Allowed only in the system mailbox".to_string());
    }

    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.disposition = Disposition::Preserved;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_list() -> Result<CommandResult, String> {
    println!("{}", command_names().join(" "));
    Ok(CommandResult::Continue)
}

fn cmd_mail(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let mut composed = ComposedMessage::new();

    for addr in args.split_whitespace() {
        // Expand aliases
        let expanded = vars.expand_alias(addr);
        if expanded.is_empty() {
            composed.add_to(addr);
        } else {
            for a in expanded {
                composed.add_to(&a);
            }
        }
    }

    // `mail` prompts for the same optional headers Send Mode does; its own
    // copy of this asked only for the subject, so `askcc`/`askbcc` were
    // silently ignored here.
    if io::stdin().is_terminal() {
        crate::send::prompt_optional_headers(&mut composed, vars)?;
    }

    compose_body(&mut composed, Some(mb), vars)?;
    send_message(&composed, vars, false)?;

    Ok(CommandResult::Continue)
}

fn cmd_mbox(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    // Allowed only in the system mailbox (spec 104853).
    if !mb.is_system_mailbox {
        return Err("mbox: Allowed only in the system mailbox".to_string());
    }

    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            // Force the message to the secondary mbox at quit, overriding a set
            // `hold` variable; this also clears any preserve mark.
            m.read = ReadState::Read;
            m.force_mbox = true;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_next(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    if !args.is_empty() {
        let msg_num = parse_message(args, mb, vars)?;
        mb.current = msg_num;
    }

    if let Some(msg) = mb.get(mb.current) {
        if msg.displayed {
            // Move to next
            if let Some(next) = mb.next_undeleted(mb.current) {
                mb.current = next;
            } else {
                println!("At EOF");
                return Ok(CommandResult::Continue);
            }
        }
    }

    cmd_print("", mb, vars, false)
}

fn cmd_pipe(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    // Parse: [msglist] command
    let (msg_nums, cmd) = if args.is_empty() {
        (
            vec![mb.current],
            vars.get("cmd").ok_or("No command specified")?.to_string(),
        )
    } else {
        // Try to find where msglist ends and command begins
        // This is tricky - we'll try parsing progressively
        let words: Vec<&str> = args.split_whitespace().collect();
        let mut msglist_end = 0;

        for i in 0..words.len() {
            let potential_msglist = words[..=i].join(" ");
            if parse_msglist(&potential_msglist, mb, false, vars).is_ok() {
                msglist_end = i + 1;
            }
        }

        if msglist_end == 0 || msglist_end >= words.len() {
            // All command or all msglist
            if let Ok(nums) = parse_msglist(args, mb, false, vars) {
                (
                    nums,
                    vars.get("cmd").ok_or("No command specified")?.to_string(),
                )
            } else {
                (vec![mb.current], args.to_string())
            }
        } else {
            let msglist = words[..msglist_end].join(" ");
            let cmd = words[msglist_end..].join(" ");
            let nums = parse_msglist(&msglist, mb, false, vars)?;
            (nums, cmd)
        }
    };

    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    let page = vars.get_bool("page");

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            let mut child = std::process::Command::new(shell)
                .arg("-c")
                .arg("--")
                .arg(&cmd)
                .stdin(std::process::Stdio::piped())
                .spawn()
                .map_err(|e| e.to_string())?;

            if let Some(stdin) = child.stdin.as_mut() {
                let content = msg.format_display(false, vars);
                stdin
                    .write_all(content.as_bytes())
                    .map_err(|e| e.to_string())?;
                if page {
                    stdin.write_all(b"\x0c").map_err(|e| e.to_string())?; // form feed
                }
            }

            child.wait().map_err(|e| e.to_string())?;
        }
    }

    mark_read(mb, &msg_nums);

    Ok(CommandResult::Continue)
}

fn cmd_print(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    show_all_headers: bool,
) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            crate::util::page_or_print(&msg.format_display(show_all_headers, vars), vars);
        }
    }

    // Mark as read and update current
    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.read = ReadState::Read;
            m.displayed = true;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.current = last;
    }

    Ok(CommandResult::Continue)
}

fn cmd_reply(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    reply_to_all: bool,
) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    for &num in &msg_nums {
        let original = mb.get(num).ok_or("No message")?;
        let mut composed = compose_reply(original, reply_to_all, vars);

        compose_body(&mut composed, Some(mb), vars)?;
        send_message(&composed, vars, false)?;
    }

    // Mark as read
    for &num in &msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.read = ReadState::Read;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.current = last;
    }

    Ok(CommandResult::Continue)
}

fn cmd_retain(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        for h in &vars.retained_headers {
            println!("{}", h);
        }
    } else {
        for header in args.split_whitespace() {
            if !vars.retained_headers.contains(&header.to_string()) {
                vars.retained_headers.push(header.to_string());
            }
        }
    }
    Ok(CommandResult::Continue)
}

fn cmd_save(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    mark_saved: bool,
) -> Result<CommandResult, String> {
    let (msg_nums, filename) = msglist_and_file(args, mb, vars, false)?;

    mb.save_messages(&msg_nums, &filename, true)?;

    if mark_saved {
        mark_messages_saved(mb, &msg_nums);
    }

    report_written(mb, &msg_nums, &filename);

    Ok(CommandResult::Continue)
}

pub(crate) fn cmd_set(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        vars.print_all();
        return Ok(CommandResult::Continue);
    }

    for arg in split_args(args) {
        if let Some(name) = variable_being_cleared(&arg, vars) {
            let name = name.to_string();
            vars.unset(&name);
            continue;
        }
        let (name, value) = parse_set_arg(&arg);
        // Reject onehop - we permanently operate in noonehop mode
        if name == "onehop" {
            return Err("onehop is not supported; operating in noonehop mode".to_string());
        }
        if let Some(val) = value {
            vars.set(name, val);
        } else {
            vars.set_bool(name, true);
        }
    }

    Ok(CommandResult::Continue)
}

/// The variable `arg` clears, if it is a `noname` form (spec 104963-104966).
///
/// `no` is stripped when what follows names a variable that is currently set,
/// or one of the internal booleans mailx knows about even when unset. Stripping
/// it from any name merely beginning with those two letters turned `set notify`
/// into `unset tify`; requiring the remainder to be a *known* boolean was the
/// opposite mistake, and stopped `set nocrt` and every user-defined variable
/// from being cleared at all.
fn variable_being_cleared<'a>(arg: &'a str, vars: &Variables) -> Option<&'a str> {
    if arg.contains('=') {
        return None;
    }
    let rest = arg.strip_prefix("no")?;
    (BOOLEAN_VARIABLES.contains(&rest) || vars.is_set(rest)).then_some(rest)
}

/// The internal variables that hold a boolean value (spec 104568-104681).
const BOOLEAN_VARIABLES: &[&str] = &[
    "allnet",
    "append",
    "ask",
    "askbcc",
    "askcc",
    "asksub",
    "autoprint",
    "bang",
    "debug",
    "dot",
    "flipr",
    "header",
    "hold",
    "ignore",
    "ignoreeof",
    "keep",
    "keepsave",
    "metoo",
    "onehop",
    "outfolder",
    "page",
    "quiet",
    "save",
    "sendwait",
    "showto",
    "verbose",
];

fn cmd_shell(cmd: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    // Expand ! to previous command if bang is set
    let cmd = if vars.get_bool("bang") {
        crate::util::expand_bang(cmd, vars.last_shell_cmd.as_deref())
    } else {
        cmd.to_string()
    };

    crate::util::shell(&cmd, vars)?;

    // Save as last command
    vars.last_shell_cmd = Some(cmd);

    println!("!");
    Ok(CommandResult::Continue)
}

fn cmd_shell_interactive(vars: &mut Variables) -> Result<CommandResult, String> {
    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    Command::new(shell).status().map_err(|e| e.to_string())?;
    Ok(CommandResult::Continue)
}

fn cmd_size(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(msg) = mb.get(num) {
            println!("{}: {} bytes", num, msg.size());
        }
    }

    Ok(CommandResult::Continue)
}

/// `source`: read commands from a file and return to command mode.
///
/// The file goes through the same interpreter as a start-up file, so its
/// `if`/`else`/`endif` work. `source` used to call the start-up dispatcher
/// directly, which recognized those words and did nothing with them -- a
/// conditional in a sourced file was silently ignored.
fn cmd_source(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let path = expand_filename(args.trim(), vars);
    let content = fs::read_to_string(&path).map_err(|e| format!("{}: {}", path, e))?;
    run_script(&content, &path, mb, vars);
    Ok(CommandResult::Continue)
}

/// Run every line of `content` as a start-up script.
///
/// Errors are reported and the script continues, which is one of the two
/// behaviors spec 104559-104561 permits. An unterminated `if` is diagnosed at
/// the end and the conditional stack is reset, so a malformed file cannot leave
/// the rest of the session suppressed.
pub fn run_script(content: &str, path: &str, mb: &mut Mailbox, vars: &mut Variables) {
    let depth = vars.cond_stack.len();
    for line in content.lines() {
        if let Err(e) = execute_in(line, mb, vars, Context::Startup) {
            eprintln!("mailx: {}: {}", path, e);
        }
    }
    if vars.cond_stack.len() > depth {
        eprintln!("mailx: {}: missing endif", path);
        vars.cond_stack.truncate(depth);
    }
}

fn cmd_top(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let toplines = vars.get_number("toplines").unwrap_or(5) as usize;

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            // Print headers
            for line in &msg.header_lines {
                println!("{}", line);
            }
            println!();

            // Print first toplines of body
            for (i, line) in msg.body.lines().enumerate() {
                if i >= toplines {
                    break;
                }
                println!("{}", line);
            }
        }
    }

    mark_read(mb, &msg_nums);

    Ok(CommandResult::Continue)
}

fn cmd_touch(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    // Allowed only in the system mailbox (spec 104853, touch grouping).
    if !mb.is_system_mailbox {
        return Err("touch: Allowed only in the system mailbox".to_string());
    }

    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            // touch marks a message read so it moves to the mbox at quit,
            // overriding a set `hold` variable (grouped with mbox, spec 104627).
            if m.read == ReadState::New || m.read == ReadState::Unread {
                m.read = ReadState::Read;
            }
            m.force_mbox = true;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_unalias(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    for name in args.split_whitespace() {
        vars.aliases.remove(name);
    }
    Ok(CommandResult::Continue)
}

fn cmd_undelete(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        // Find deleted message
        if let Some(num) = mb.next_deleted(mb.current.saturating_sub(1)) {
            vec![num]
        } else if let Some(num) = mb.prev_deleted(mb.current + 1) {
            vec![num]
        } else {
            return Err("No deleted messages".to_string());
        }
    } else {
        parse_msglist(args, mb, true, vars)?
    };

    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            if m.disposition == Disposition::Deleted {
                // Clear the deletion and leave the read state alone. Undeleting
                // used to mark the message read, so a message that arrived new,
                // was deleted, and was undeleted came back as already read.
                m.disposition = Disposition::Keep;
            }
        }
    }

    if let Some(&last) = msg_nums.last() {
        mb.current = last;
    }

    if vars.get_bool("autoprint") {
        cmd_print("", mb, vars, false)?;
    }

    Ok(CommandResult::Continue)
}

pub(crate) fn cmd_unset(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    for name in args.split_whitespace() {
        vars.unset(name);
    }
    Ok(CommandResult::Continue)
}

fn cmd_visual(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let editor = vars.get("VISUAL").unwrap_or("vi");
    edit_messages(&msg_nums, mb, editor)?;

    Ok(CommandResult::Continue)
}

fn cmd_write(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let (msg_nums, filename) = msglist_and_file(args, mb, vars, true)?;

    // Write without headers
    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&filename)
        .map_err(|e| format!("{}: {}", filename, e))?;

    let mut total_size = 0;
    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            file.write_all(msg.body.as_bytes())
                .map_err(|e| e.to_string())?;
            total_size += msg.body.len();
        }
    }

    mark_messages_saved(mb, &msg_nums);

    println!(
        "\"{}\" {} messages {} bytes",
        filename,
        msg_nums.len(),
        total_size
    );

    Ok(CommandResult::Continue)
}

fn cmd_scroll(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
    let screen = vars.screen_lines().get();
    let direction = if args.starts_with('-') { -1i32 } else { 1 };

    let current_page = (mb.current.saturating_sub(1)) / screen;
    let new_page = if direction > 0 {
        current_page + 1
    } else {
        current_page.saturating_sub(1)
    };

    let start = new_page * screen + 1;
    if start > mb.message_count() {
        println!("On last screenful of messages");
        return Ok(CommandResult::Continue);
    }

    let end = mb.message_count().min(start + screen - 1);
    let nums: Vec<usize> = (start..=end).collect();

    if !nums.is_empty() {
        mb.current = nums[0];
    }

    mb.print_headers(Some(&nums), vars);

    Ok(CommandResult::Continue)
}

// ============ Helper functions ============

/// Resolve a filename typed in command mode.
///
/// POSIX applies two transformations in sequence (spec 104704-104711): a
/// leading unquoted `+` becomes the `folder` variable and a slash, then shell
/// word expansion. mailx's own `%`, `%user`, and `&` notations are resolved
/// first; `#` needs the previous folder, so `cmd_file` handles it.
///
/// Only one pathname may result. The old expansion interpolated the name into
/// `sh -c "printf '%s' <name>"` unquoted and joined whatever came back, so
/// `my file.txt` silently became `myfile.txt`; a name the shell could not parse
/// fell back to itself with no diagnostic.
fn expand_filename(name: &str, vars: &Variables) -> String {
    let expanded = expand_local_prefixes(name, vars);
    shell_expand(&expanded, vars).unwrap_or(expanded)
}

/// Resolve a filename derived from message content, without the shell.
///
/// `Save`, `Copy`, `followup`, and `Followup` name their file after the
/// message author. That name is attacker-controlled, so it gets the `+folder`
/// and `%`/`&` notations that are mailx's own, and stops there -- the shell
/// word expansion POSIX applies to a *typed* filename (spec 104709) would make
/// a `From:` header a command line.
fn expand_author_filename(name: &str, vars: &Variables) -> String {
    expand_local_prefixes(name, vars)
}

/// Apply shell word expansion to a filename, requiring a single result.
///
/// `printf '%s\n'` puts each resulting word on its own line, so a name that
/// expands to several pathnames can be recognized rather than silently
/// concatenated. Returns `None` when the name is left as typed.
fn shell_expand(name: &str, vars: &Variables) -> Option<String> {
    // The word is interpolated into a shell command, so this must only ever be
    // reached for a name the user typed. A filename derived from message
    // content goes through `expand_author_filename`, which stops short of the
    // shell.
    // Nothing to expand: no shell metacharacter, no `~`, no variable.
    if !name.contains(['~', '$', '*', '?', '[', '`', '"', '\'', '\\']) {
        return None;
    }

    let out = crate::util::shell_output(&format!("printf '%s\\n' {}", name), vars).ok()?;
    let mut words = out.lines();
    let first = words.next()?;
    if words.next().is_some() {
        // More than one pathname where one is expected: effects are unspecified
        // (spec 104710-104711), so leave the name alone rather than guess.
        return None;
    }
    Some(first.to_string())
}

/// Handle a `SIGINT` received while composing a message in input mode.
///
/// With `ignore` set, the interrupt prints `@` and discards the current line.
/// Otherwise the first interrupt warns; a second consecutive interrupt aborts
/// the message, writing the partial letter to the dead-letter file when `save`
/// is set.  Returns `true` when the message should be aborted.
pub(crate) fn interrupt_message(
    composed: &ComposedMessage,
    vars: &Variables,
    interrupt_count: &mut u32,
) -> bool {
    if vars.get_bool("ignore") {
        println!("@");
        return false;
    }
    *interrupt_count += 1;
    if *interrupt_count >= 2 {
        if vars.get_bool("save") && !composed.body.is_empty() {
            crate::send::save_dead_letter(composed, vars);
        }
        return true;
    }
    println!("(Interrupt -- one more to kill letter)");
    false
}

/// Read a message body from standard input, handling command escapes.
///
/// Shared by every entry into input mode: `mail`, `reply`, `followup`, and Send
/// Mode. There were two copies of this loop, and they had diverged -- the one
/// used by `mail`/`reply`/`followup` ignored the escape result's request to
/// save the partial message, so `~q` from those commands never wrote
/// `dead.letter` although `~q` in Send Mode did.
pub(crate) fn compose_body(
    composed: &mut ComposedMessage,
    mb: Option<&Mailbox>,
    vars: &mut Variables,
) -> Result<(), String> {
    let escape_char = vars.escape_char();
    let mut interrupt_count = 0;

    loop {
        let mut line = String::new();
        match crate::signals::read_line_interruptible(&mut line) {
            Ok(0) => {
                // EOF - if ignoreeof is set, require "." (or ~.) to end.
                if vars.get_bool("ignoreeof") {
                    println!("Use \".\" to terminate letter.");
                    continue;
                }
                break;
            }
            Ok(_) => {}
            Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                crate::signals::take_sigint();
                if interrupt_message(composed, vars, &mut interrupt_count) {
                    return Err("Interrupt".to_string());
                }
                continue;
            }
            Err(e) => return Err(e.to_string()),
        }

        // A SIGINT may have arrived between reads.
        if crate::signals::take_sigint() && interrupt_message(composed, vars, &mut interrupt_count)
        {
            return Err("Interrupt".to_string());
        }

        interrupt_count = 0;

        // Check for escape character (disabled when `escape` is null). Slice
        // past the escape char by its UTF-8 length so a multibyte escape does
        // not split a character boundary.
        if let Some(ec) =
            escape_char.filter(|ec| line.starts_with(*ec) && line.len() > ec.len_utf8())
        {
            // A tilde-escape error is diagnosed but does not abort the message
            // (spec 105114-105119).
            let result = match handle_escape(&line[ec.len_utf8()..], composed, vars, mb) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("{}", e);
                    continue;
                }
            };
            if result.done {
                if result.abort {
                    // `~q` saves the partial message, `~x` does not
                    // (spec 105081-105083, 105097).
                    if result.save_dead_letter && vars.get_bool("save") && !composed.body.is_empty()
                    {
                        crate::send::save_dead_letter(composed, vars);
                    }
                    return Err("Aborted".to_string());
                }
                break;
            }
            continue;
        }

        // Check for single period (if dot is set, or ignoreeof forces it)
        if (vars.get_bool("dot") || vars.get_bool("ignoreeof")) && line.trim() == "." {
            break;
        }

        composed.body.push_str(&line);
    }

    Ok(())
}
