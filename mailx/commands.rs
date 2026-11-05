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

use crate::compose::compose_body;
use crate::error::{IoContext, Result};
use crate::filename::{expand_local_prefixes, expand_typed_filename};
use crate::mailbox::{Direction, Mailbox};
use crate::message::{author_filename, Disposition, Message, ReadState};
use crate::msglist::{
    current_message, is_positional, msglist_or_current, parse_message, parse_msglist, Want,
};
use crate::script::CondStack;
use crate::send::{compose_reply, send_message, ComposedMessage};
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

/// Interpreter state that outlives a single command line.
#[derive(Debug, Default)]
pub struct Interp {
    /// The `if`/`else`/`endif` blocks being executed.
    pub cond: CondStack,
    /// Whether mailx is in Send Mode, which `if s` and `if r` test.
    pub send_mode: bool,
    /// The last `!` or `~!` command, for `bang` expansion.
    pub last_shell_cmd: Option<String>,
}

type Handler = fn(&str, &mut Mailbox, &mut Variables, &mut Interp) -> Result<CommandResult>;

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

const fn cmd(name: &'static str, min: usize, ctx: u8, run: Handler) -> Cmd {
    Cmd {
        name,
        min,
        ctx,
        run,
    }
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
    cmd("alias", 1, ANY, |a, _, v, _| cmd_alias(a, v)),
    cmd("group", 1, ANY, |a, _, v, _| cmd_alias(a, v)),
    cmd("alternates", 3, ANY, |a, _, v, _| cmd_alternates(a, v)),
    cmd("echo", 2, ANY, |a, _, _, _| {
        println!("{}", a);
        Ok(CommandResult::Continue)
    }),
    cmd("set", 2, ANY, |a, _, v, _| cmd_set(a, v)),
    cmd("unset", 3, ANY, |a, _, v, _| cmd_unset(a, v)),
    cmd("cd", 2, CMD_RC, |a, _, _, _| cmd_cd(a)),
    cmd("chdir", 2, CMD_RC, |a, _, _, _| cmd_cd(a)),
    cmd("copy", 1, CMD_RC, |a, m, v, _| cmd_save(a, m, v, false)),
    cmd("Copy", 1, CMD, |a, m, v, _| cmd_copy_author(a, m, v, false)),
    cmd("delete", 1, CMD_RC, |a, m, v, _| cmd_delete(a, m, v)),
    cmd("discard", 2, CMD_RC, |a, _, v, _| cmd_discard(a, v)),
    cmd("ignore", 2, CMD_RC, |a, _, v, _| cmd_discard(a, v)),
    cmd("dp", 2, CMD_RC, |a, m, v, _| cmd_dp(a, m, v)),
    cmd("dt", 2, CMD_RC, |a, m, v, _| cmd_dp(a, m, v)),
    cmd("edit", 1, CMD, |a, m, v, _| cmd_edit(a, m, v)),
    cmd("exit", 2, CMD_RC, |_, _, _, _| Ok(CommandResult::Exit)),
    cmd("xit", 1, CMD_RC, |_, _, _, _| Ok(CommandResult::Exit)),
    cmd("file", 2, CMD_RC, |a, m, v, _| cmd_file(a, m, v)),
    cmd("folder", 4, CMD_RC, |a, m, v, _| cmd_file(a, m, v)),
    cmd("folders", 7, CMD_RC, |_, _, v, _| cmd_folders(v)),
    cmd("followup", 2, CMD, |a, m, v, i| {
        cmd_followup(a, m, v, i, false)
    }),
    cmd("Followup", 2, CMD, |a, m, v, i| {
        cmd_followup(a, m, v, i, true)
    }),
    cmd("from", 1, CMD_RC, |a, m, v, _| cmd_from(a, m, v)),
    cmd("headers", 1, CMD_RC, |a, m, v, _| cmd_headers(a, m, v)),
    cmd("help", 3, CMD_RC, |_, _, _, _| cmd_help()),
    cmd("hold", 2, CMD, |a, m, v, _| cmd_hold(a, m, v)),
    cmd("preserve", 3, CMD, |a, m, v, _| cmd_hold(a, m, v)),
    cmd("list", 1, CMD_RC, |_, _, _, _| cmd_list()),
    cmd("mail", 1, CMD, cmd_mail),
    cmd("mbox", 2, CMD_RC, |a, m, v, _| cmd_mbox(a, m, v)),
    cmd("next", 1, CMD_RC, |a, m, v, _| cmd_next(a, m, v)),
    cmd("pipe", 2, CMD_RC, |a, m, v, _| cmd_pipe(a, m, v)),
    cmd("print", 1, CMD_RC, |a, m, v, _| cmd_print(a, m, v, false)),
    cmd("Print", 1, CMD_RC, |a, m, v, _| cmd_print(a, m, v, true)),
    cmd("type", 1, CMD_RC, |a, m, v, _| cmd_print(a, m, v, false)),
    cmd("Type", 1, CMD_RC, |a, m, v, _| cmd_print(a, m, v, true)),
    cmd("quit", 1, CMD_RC, |_, _, _, _| Ok(CommandResult::Quit)),
    cmd("reply", 1, CMD, |a, m, v, i| {
        cmd_reply_flipr(a, m, v, i, true)
    }),
    cmd("respond", 3, CMD, |a, m, v, i| {
        cmd_reply_flipr(a, m, v, i, true)
    }),
    cmd("Reply", 1, CMD, |a, m, v, i| {
        cmd_reply_flipr(a, m, v, i, false)
    }),
    cmd("Respond", 3, CMD, |a, m, v, i| {
        cmd_reply_flipr(a, m, v, i, false)
    }),
    cmd("retain", 3, CMD_RC, |a, _, v, _| cmd_retain(a, v)),
    cmd("save", 1, CMD_RC, |a, m, v, _| cmd_save(a, m, v, true)),
    cmd("Save", 1, CMD, |a, m, v, _| cmd_copy_author(a, m, v, true)),
    cmd("shell", 2, CMD, |_, _, v, _| cmd_shell_interactive(v)),
    cmd("size", 2, CMD_RC, |a, m, v, _| cmd_size(a, m, v)),
    cmd("source", 2, CMD_RC, cmd_source),
    cmd("top", 2, CMD_RC, |a, m, v, _| cmd_top(a, m, v)),
    cmd("touch", 3, CMD_RC, |a, m, v, _| cmd_touch(a, m, v)),
    cmd("unalias", 3, CMD_RC, |a, _, v, _| cmd_unalias(a, v)),
    cmd("undelete", 1, CMD_RC, |a, m, v, _| cmd_undelete(a, m, v)),
    cmd("visual", 1, CMD, |a, m, v, _| cmd_visual(a, m, v)),
    cmd("write", 1, CMD_RC, |a, m, v, _| cmd_write(a, m, v)),
];

/// Commands spelled with punctuation rather than a name.
const PUNCTUATION: &[Cmd] = &[
    cmd("?", 1, CMD_RC, |_, _, _, _| cmd_help()),
    cmd("|", 1, CMD_RC, |a, m, v, _| cmd_pipe(a, m, v)),
];

/// Find the command `word` names, if any.
///
/// A command may be entered as any prefix of its name at least `min` characters
/// long (spec 104689-104693). The comparison is case-sensitive.
fn lookup(word: &str, ctx: Context) -> Option<Result<&'static Cmd>> {
    let cmd = COMMANDS
        .iter()
        .find(|c| word.len() >= c.min && c.name.len() >= word.len() && c.name.starts_with(word))?;
    Some(if cmd.ctx & ctx.bit() == 0 {
        Err(format!("{}: {}", word, ctx.where_invalid()).into())
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
    interp: &mut Interp,
    lowercase_form: bool,
) -> Result<CommandResult> {
    let reply_to_all = lowercase_form != vars.is_set("flipr");
    cmd_reply(args, mb, vars, interp, reply_to_all)
}

/// Execute a command in Receive Mode
pub fn execute_command(
    line: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    interp: &mut Interp,
) -> Result<CommandResult> {
    execute_in(line, mb, vars, interp, Context::Command)
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
    interp: &mut Interp,
    ctx: Context,
) -> Result<CommandResult> {
    let line = line.trim();

    if line.is_empty() || line.starts_with('#') {
        return Ok(CommandResult::Continue);
    }

    let (cmd, args) = parse_command_line(line);

    // Conditionals are handled before the context check, so an `if` guarding a
    // command that is illegal here still suppresses it rather than diagnosing
    // it.
    if let Some(result) = crate::script::conditional(cmd, args, interp) {
        return result.map(|_| CommandResult::Continue);
    }
    if !interp.cond.active() {
        return Ok(CommandResult::Continue);
    }

    // Shell escape. Not valid in a start-up file (spec 104557-104559).
    if let Some(rest) = line.strip_prefix('!') {
        if ctx == Context::Startup {
            return Err("!: command not valid in a start-up file".into());
        }
        return cmd_shell(rest, vars, interp);
    }

    // Current message number.
    if line == "=" {
        println!("{}", mb.current().unwrap_or(0));
        return Ok(CommandResult::Continue);
    }

    if let Some(punct) = PUNCTUATION.iter().find(|c| c.name == cmd) {
        if punct.ctx & ctx.bit() == 0 {
            return Err(format!("{}: {}", cmd, ctx.where_invalid()).into());
        }
        return (punct.run)(args, mb, vars, interp);
    }

    // Scrolling: `z`, `z+`, `z-`.
    if let Some(direction) = cmd.strip_prefix('z') {
        if direction.is_empty() || direction == "+" || direction == "-" {
            if ctx != Context::Command {
                return Err(format!("{}: {}", cmd, ctx.where_invalid()).into());
            }
            return cmd_scroll(direction, mb, vars);
        }
    }

    if let Some(found) = lookup(cmd, ctx) {
        return (found?.run)(args, mb, vars, interp);
    }

    // A bare message number selects and prints that message.
    if let Ok(num) = cmd.parse::<usize>() {
        if ctx != Context::Command {
            return Err(format!("{}: {}", cmd, ctx.where_invalid()).into());
        }
        return if num > 0 && num <= mb.message_count() {
            mb.set_current(num);
            cmd_print("", mb, vars, false)
        } else {
            Err(format!("Invalid message number: {}", num).into())
        };
    }

    Err(format!("Unknown command: {}", cmd).into())
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

/// Report what a `save`, `copy`, `Save`, or `Copy` wrote.
fn report_written(msg_nums: &[usize], filename: &str, total: u64) {
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
}

/// Mark messages read and make the last of them current.
fn mark_read(mb: &mut Mailbox, msg_nums: &[usize]) {
    for &num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.read = ReadState::Read;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.set_current(last);
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
) -> Result<(Vec<usize>, String)> {
    let parts: Vec<&str> = args.split_whitespace().collect();
    match parts.len() {
        0 => {
            if file_required {
                return Err("No file specified".into());
            }
            Ok((current_message(mb)?, crate::util::mbox_path(vars)))
        }
        1 => match parse_msglist(parts[0], mb, Want::Live, vars) {
            Ok(nums) if !file_required => Ok((nums, crate::util::mbox_path(vars))),
            _ => Ok((current_message(mb)?, expand_typed_filename(parts[0], vars))),
        },
        n => {
            let nums = parse_msglist(&parts[..n - 1].join(" "), mb, Want::Live, vars)?;
            Ok((nums, expand_typed_filename(parts[n - 1], vars)))
        }
    }
}

pub(crate) fn cmd_alias(args: &str, vars: &mut Variables) -> Result<CommandResult> {
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

pub(crate) fn cmd_alternates(args: &str, vars: &mut Variables) -> Result<CommandResult> {
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

fn cmd_cd(args: &str) -> Result<CommandResult> {
    let dir = if args.is_empty() {
        crate::util::home()
    } else {
        args.to_string()
    };

    env::set_current_dir(&dir).context(&dir)?;
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
) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    if let Some(first_msg) = msg_nums.first().and_then(|&n| mb.get(n)) {
        let filename = author_filename(first_msg.from())?;
        let filename = expand_local_prefixes(&filename, vars);

        let written = mb.save_messages(&msg_nums, &filename, true)?;

        if mark_saved {
            mark_messages_saved(mb, &msg_nums);
        }

        report_written(&msg_nums, &filename, written);
    }

    Ok(CommandResult::Continue)
}

fn cmd_delete(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let max_deleted = msg_nums.iter().copied().max().unwrap_or(0);

    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.disposition = Disposition::Deleted;
        }
    }

    let live = |m: &Message| !m.is_deleted();
    if let Some(next) = mb.find(max_deleted, Direction::Forward, live) {
        mb.set_current(next);
    } else if let Some(prev) = mb.find(max_deleted, Direction::Backward, live) {
        mb.set_current(prev);
    }

    if vars.is_set("autoprint") && mb.current_message().is_some_and(live) {
        cmd_print("", mb, vars, false)?;
    }

    Ok(CommandResult::Continue)
}

fn cmd_discard(args: &str, vars: &mut Variables) -> Result<CommandResult> {
    if args.is_empty() {
        for field in vars.header_filter.ignored() {
            println!("{}", field);
        }
    } else {
        for field in args.split_whitespace() {
            vars.header_filter.ignore(field);
        }
    }
    Ok(CommandResult::Continue)
}

fn cmd_dp(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    // Delete and print next
    cmd_delete(args, mb, vars)?;

    if mb.current_message().is_some_and(|m| !m.is_deleted()) {
        cmd_print("", mb, vars, false)?;
    } else {
        println!("At EOF");
    }

    Ok(CommandResult::Continue)
}

fn cmd_edit(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let editor = vars.get_nonempty("EDITOR").unwrap_or("ed");
    edit_messages(&msg_nums, mb, editor)
}

/// Run `editor` over each message in turn, on a private temporary copy.
///
/// The temporary comes from `mkstemp` (`O_EXCL`, mode 0600, honoring `$TMPDIR`)
/// rather than a name built from the pid: `/tmp/mailx.<pid>.<n>` is predictable,
/// world-readable, and `fs::write` follows a symlink planted there first.
fn edit_messages(msg_nums: &[usize], mb: &Mailbox, editor: &str) -> Result<CommandResult> {
    for &num in msg_nums {
        if let Some(msg) = mb.get(num) {
            let mut tmp = plib::tmp::NamedTempFile::new().map_err(|e| e.to_string())?;
            tmp.as_file_mut()
                .write_all(&msg.format_full())
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

fn cmd_file(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    if args.is_empty() {
        // Print current mailbox info
        println!("\"{}\" {} messages", mb.path, mb.undeleted_count());
        return Ok(CommandResult::Continue);
    }

    // Resolve the target, handling `#` (the previously-opened folder).
    let path = if args.trim() == "#" {
        match &mb.prev_path {
            Some(p) => p.clone(),
            None => return Err("No previous folder".into()),
        }
    } else {
        expand_typed_filename(args, vars)
    };

    // Remember the folder we are leaving for a later `#`.
    let leaving = mb.path.clone();

    // Check the new folder can be opened *before* flushing the old one, so a
    // bad path does not leave the session pointing at a mailbox already written
    // out. The load itself has to happen after the flush: reading it first
    // would record a byte length taken before `quit` rewrote the file, and if
    // the two are the same mailbox everything `quit` appended would then look
    // like newly delivered mail and be copied through a second time.
    fs::File::open(&path).context(&path)?;
    mb.quit(vars)?;
    let mut new_mb = Mailbox::load(&path)?;
    new_mb.prev_path = Some(leaving);

    *mb = new_mb;
    crate::summary::print_headers(mb, None, vars);

    Ok(CommandResult::Continue)
}

fn cmd_folders(vars: &mut Variables) -> Result<CommandResult> {
    if let Some(folder) = vars.get_nonempty("folder") {
        let folder = expand_typed_filename(folder, vars);
        let lister = vars.get_nonempty("LISTER").unwrap_or("ls");
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
    interp: &mut Interp,
    reply_to_all: bool,
) -> Result<CommandResult> {
    let msg_num = if args.is_empty() {
        mb.current().ok_or("No message")?
    } else {
        parse_message(args, mb, vars)?
    };

    let original = mb.get(msg_num).ok_or("No message")?;
    let mut composed = compose_reply(original, reply_to_all, vars);

    // Record to file named after first recipient
    let record_file = author_filename(original.from())?;
    let record_file = expand_local_prefixes(&record_file, vars);

    // Enter input mode
    compose_body(&mut composed, Some(mb), vars, interp)?;

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

fn cmd_from(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    crate::summary::print_headers(mb, Some(&msg_nums), vars);
    Ok(CommandResult::Continue)
}

fn cmd_headers(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    if !args.is_empty() {
        let msg_num = parse_message(args, mb, vars)?;
        mb.set_current(msg_num);
    }

    let screen = vars.screen_lines().get();
    let start = mb.current().map_or(1, |c| (c - 1) / screen * screen + 1);

    let nums: Vec<usize> = (start..=mb.message_count().min(start + screen - 1)).collect();
    crate::summary::print_headers(mb, Some(&nums), vars);

    Ok(CommandResult::Continue)
}

/// The command list, generated from the dispatch table.
///
/// `help` and `list` used to carry hand-written inventories. They had already
/// drifted from what dispatch accepted: `list` advertised `Copy`, `Save`,
/// `Print`, `Type`, `Reply`, and `Followup`, none of which could run.
fn command_names() -> Vec<&'static str> {
    let mut names: Vec<&'static str> = COMMANDS.iter().map(|c| c.name).collect();
    names.extend(PUNCTUATION.iter().map(|c| c.name));
    names.push("z");
    names
}

fn cmd_help() -> Result<CommandResult> {
    println!("Commands:");
    for chunk in command_names().chunks(6) {
        println!("  {}", chunk.join("  "));
    }
    println!("Any command may be abbreviated to its shortest unique prefix.");
    Ok(CommandResult::Continue)
}

fn cmd_hold(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    // Allowed only in the system mailbox (spec 104831).
    if !mb.is_system_mailbox {
        return Err("hold: Allowed only in the system mailbox".into());
    }

    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.disposition = Disposition::Preserved;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_list() -> Result<CommandResult> {
    println!("{}", command_names().join(" "));
    Ok(CommandResult::Continue)
}

fn cmd_mail(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    interp: &mut Interp,
) -> Result<CommandResult> {
    let mut composed = ComposedMessage::new();

    for addr in args.split_whitespace().flat_map(|a| vars.recipients(a)) {
        composed.add_to(&addr);
    }

    // `mail` prompts for the same optional headers Send Mode does; its own
    // copy of this asked only for the subject, so `askcc`/`askbcc` were
    // silently ignored here.
    if io::stdin().is_terminal() {
        crate::send::prompt_optional_headers(&mut composed, vars)?;
    }

    compose_body(&mut composed, Some(mb), vars, interp)?;
    send_message(&composed, vars, false)?;

    Ok(CommandResult::Continue)
}

fn cmd_mbox(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    // Allowed only in the system mailbox (spec 104853).
    if !mb.is_system_mailbox {
        return Err("mbox: Allowed only in the system mailbox".into());
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

fn cmd_next(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    if !args.is_empty() {
        let msg_num = parse_message(args, mb, vars)?;
        mb.set_current(msg_num);
    }

    if let (Some(current), Some(msg)) = (mb.current(), mb.current_message()) {
        if msg.displayed {
            if let Some(next) = mb.find(current, Direction::Forward, |m| !m.is_deleted()) {
                mb.set_current(next);
            } else {
                println!("At EOF");
                return Ok(CommandResult::Continue);
            }
        }
    }

    cmd_print("", mb, vars, false)
}

/// Split `pipe` arguments into the messages to pipe and the command.
///
/// POSIX makes the command the last argument, quoted if it holds blanks;
/// arguments that are all message specifications pipe those messages
/// through `cmd`. An unquoted command is accepted too, after any leading
/// positional specifications.
fn pipe_arguments(args: &str, mb: &Mailbox, vars: &Variables) -> Result<(Vec<usize>, String)> {
    let words = split_args(args);
    let Some((command, list)) = words
        .split_last()
        .filter(|_| !words.iter().all(|w| is_positional(w)))
    else {
        let cmd = vars.get_nonempty("cmd").ok_or("No command specified")?;
        return Ok((msglist_or_current(args, mb, vars)?, cmd.to_string()));
    };
    let listed = if list.is_empty() {
        current_message(mb)
    } else {
        parse_msglist(&list.join(" "), mb, Want::Live, vars)
    };
    if let Ok(msgs) = listed {
        return Ok((msgs, command.clone()));
    }

    let leading: Vec<&str> = args
        .split_whitespace()
        .take_while(|w| is_positional(w))
        .collect();
    let msgs = msglist_or_current(&leading.join(" "), mb, vars)?;
    Ok((msgs, after_words(args, leading.len()).to_string()))
}

/// `args` after its first `n` blank-separated words.
fn after_words(args: &str, n: usize) -> &str {
    let mut rest = args.trim_start();
    for _ in 0..n {
        let end = rest.find(char::is_whitespace).unwrap_or(rest.len());
        rest = rest[end..].trim_start();
    }
    rest
}

fn cmd_pipe(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let (msg_nums, cmd) = pipe_arguments(args, mb, vars)?;

    let page = vars.is_set("page");

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            let mut child = crate::util::shell_command(&cmd, vars)
                .stdin(std::process::Stdio::piped())
                .spawn()
                .map_err(|e| e.to_string())?;

            // As the pager above: quitting it closes this pipe.
            let _sigpipe = plib::io::SigPipeIgnored::new();

            if let Some(stdin) = child.stdin.as_mut() {
                let content = msg.format_display(false, vars);
                stdin.write_all(&content).map_err(|e| e.to_string())?;
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
) -> Result<CommandResult> {
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
        mb.set_current(last);
    }

    Ok(CommandResult::Continue)
}

fn cmd_reply(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    interp: &mut Interp,
    reply_to_all: bool,
) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    for &num in &msg_nums {
        let original = mb.get(num).ok_or("No message")?;
        let mut composed = compose_reply(original, reply_to_all, vars);

        compose_body(&mut composed, Some(mb), vars, interp)?;
        send_message(&composed, vars, false)?;
    }

    // Mark as read
    for &num in &msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.read = ReadState::Read;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.set_current(last);
    }

    Ok(CommandResult::Continue)
}

fn cmd_retain(args: &str, vars: &mut Variables) -> Result<CommandResult> {
    if args.is_empty() {
        for field in vars.header_filter.retained() {
            println!("{}", field);
        }
    } else {
        for field in args.split_whitespace() {
            vars.header_filter.retain(field);
        }
    }
    Ok(CommandResult::Continue)
}

/// `save` and `copy`: append messages to a file, mbox when none is named.
/// `copy` differs only in leaving the messages unmarked.
fn cmd_save(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    mark_saved: bool,
) -> Result<CommandResult> {
    let (msg_nums, filename) = msglist_and_file(args, mb, vars, false)?;

    let written = mb.save_messages(&msg_nums, &filename, true)?;

    if mark_saved {
        mark_messages_saved(mb, &msg_nums);
    }

    report_written(&msg_nums, &filename, written);

    Ok(CommandResult::Continue)
}

pub(crate) fn cmd_set(args: &str, vars: &mut Variables) -> Result<CommandResult> {
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
            return Err("onehop is not supported; operating in noonehop mode".into());
        }
        vars.set(name, value.unwrap_or(""));
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

fn cmd_shell(cmd: &str, vars: &mut Variables, interp: &mut Interp) -> Result<CommandResult> {
    let cmd = if vars.is_set("bang") {
        crate::util::expand_bang(cmd, interp.last_shell_cmd.as_deref())
    } else {
        cmd.to_string()
    };

    crate::util::shell(&cmd, vars)?;

    interp.last_shell_cmd = Some(cmd);

    println!("!");
    Ok(CommandResult::Continue)
}

fn cmd_shell_interactive(vars: &mut Variables) -> Result<CommandResult> {
    Command::new(crate::util::shell_program(vars))
        .status()
        .map_err(|e| e.to_string())?;
    Ok(CommandResult::Continue)
}

fn cmd_size(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
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
fn cmd_source(
    args: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
    interp: &mut Interp,
) -> Result<CommandResult> {
    let path = expand_typed_filename(args.trim(), vars);
    let content = fs::read_to_string(&path).context(&path)?;
    crate::script::run_script(&content, &path, mb, vars, interp);
    Ok(CommandResult::Continue)
}

fn cmd_top(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let toplines = vars.get_number("toplines").unwrap_or(5) as usize;

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            // Print headers
            let mut out = Vec::new();
            for line in &msg.header_lines {
                out.extend_from_slice(line);
                out.push(b'\n');
            }
            out.push(b'\n');

            // Print first toplines of body
            for line in crate::util::byte_lines(&msg.body).take(toplines) {
                out.extend_from_slice(line);
                out.push(b'\n');
            }
            crate::util::print_bytes(&out);
        }
    }

    mark_read(mb, &msg_nums);

    Ok(CommandResult::Continue)
}

fn cmd_touch(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    // Allowed only in the system mailbox (spec 104853, touch grouping).
    if !mb.is_system_mailbox {
        return Err("touch: Allowed only in the system mailbox".into());
    }

    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            // touch marks a message read so it moves to the mbox at quit,
            // overriding a set `hold` variable (grouped with mbox, spec 104627).
            if m.read.is_unread() {
                m.read = ReadState::Read;
            }
            m.force_mbox = true;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_unalias(args: &str, vars: &mut Variables) -> Result<CommandResult> {
    for name in args.split_whitespace() {
        vars.aliases.remove(name);
    }
    Ok(CommandResult::Continue)
}

fn cmd_undelete(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let msg_nums = if args.is_empty() {
        let current = mb.current().unwrap_or(0);
        let deleted = Message::is_deleted;
        if let Some(num) = mb.find(current.saturating_sub(1), Direction::Forward, deleted) {
            vec![num]
        } else if let Some(num) = mb.find(current + 1, Direction::Backward, deleted) {
            vec![num]
        } else {
            return Err("No deleted messages".into());
        }
    } else {
        parse_msglist(args, mb, Want::Deleted, vars)?
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
        mb.set_current(last);
    }

    if vars.is_set("autoprint") {
        cmd_print("", mb, vars, false)?;
    }

    Ok(CommandResult::Continue)
}

pub(crate) fn cmd_unset(args: &str, vars: &mut Variables) -> Result<CommandResult> {
    for name in args.split_whitespace() {
        vars.unset(name);
    }
    Ok(CommandResult::Continue)
}

fn cmd_visual(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let editor = vars.get_nonempty("VISUAL").unwrap_or("vi");
    edit_messages(&msg_nums, mb, editor)?;

    Ok(CommandResult::Continue)
}

fn cmd_write(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let (msg_nums, filename) = msglist_and_file(args, mb, vars, true)?;

    // Write without headers
    let mut file = crate::util::private_open()
        .create(true)
        .append(true)
        .open(&filename)
        .context(&filename)?;

    let mut total_size = 0;
    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            file.write_all(&msg.body).map_err(|e| e.to_string())?;
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

fn cmd_scroll(args: &str, mb: &mut Mailbox, vars: &mut Variables) -> Result<CommandResult> {
    let screen = vars.screen_lines().get();
    let direction = if args.starts_with('-') { -1i32 } else { 1 };

    let current_page = mb.current().map_or(0, |c| (c - 1) / screen);
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
        mb.set_current(nums[0]);
    }

    crate::summary::print_headers(mb, Some(&nums), vars);

    Ok(CommandResult::Continue)
}

// ============ Helper functions ============

#[cfg(test)]
mod tests {
    use super::COMMANDS;

    /// `lookup` takes the first command that accepts a word, so no word may
    /// be accepted by two of them: the table order must not matter.
    #[test]
    fn no_word_names_two_commands() {
        for (i, a) in COMMANDS.iter().enumerate() {
            for b in &COMMANDS[i + 1..] {
                let common = a
                    .name
                    .bytes()
                    .zip(b.name.bytes())
                    .take_while(|(x, y)| x == y)
                    .count();
                assert!(
                    common < a.min.max(b.min),
                    "`{}` and `{}` both accept `{}`",
                    a.name,
                    b.name,
                    &a.name[..common]
                );
            }
        }
    }
}
