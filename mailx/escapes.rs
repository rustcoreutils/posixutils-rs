//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Tilde escape handling for mailx input mode

use std::env;
use std::fs;
use std::io::{self, IsTerminal, Write};
use std::process::{Command, Stdio};

use crate::mailbox::Mailbox;
use crate::msglist::msglist_or_current;
use crate::send::ComposedMessage;
use crate::variables::Variables;

/// Result of handling an escape
pub struct EscapeResult {
    pub done: bool,
    pub abort: bool,
    /// Whether to save to dead letter on abort (true for ~q, false for ~x)
    pub save_dead_letter: bool,
}

impl EscapeResult {
    pub fn continue_input() -> Self {
        EscapeResult {
            done: false,
            abort: false,
            save_dead_letter: false,
        }
    }

    pub fn finish() -> Self {
        EscapeResult {
            done: true,
            abort: false,
            save_dead_letter: false,
        }
    }

    /// Abort and save to dead letter (~q behavior)
    pub fn abort_with_save() -> Self {
        EscapeResult {
            done: true,
            abort: true,
            save_dead_letter: true,
        }
    }

    /// Abort without saving (~x behavior)
    pub fn abort_without_save() -> Self {
        EscapeResult {
            done: true,
            abort: true,
            save_dead_letter: false,
        }
    }
}

/// Handle an escape command
pub fn handle_escape(
    line: &str,
    msg: &mut ComposedMessage,
    vars: &mut Variables,
    mb: Option<&Mailbox>,
) -> Result<EscapeResult, String> {
    if line.is_empty() {
        // Just the escape char - treat as literal
        return Ok(EscapeResult::continue_input());
    }

    let cmd_char = line.chars().next().unwrap();
    // Slice past the command character by its UTF-8 length. Slicing at byte 1
    // split a multibyte character (`~élan`) and panicked -- the callers already
    // take care to advance by `len_utf8` past the escape character itself.
    let args = line[cmd_char.len_utf8()..].trim();

    match cmd_char {
        '.' => {
            // End message input
            Ok(EscapeResult::finish())
        }
        '!' => {
            // Shell escape with bang expansion
            let cmd = if vars.get_bool("bang") {
                // Expand ! to previous command
                crate::util::expand_bang(args, vars.last_shell_cmd.as_deref())
            } else {
                args.to_string()
            };

            crate::util::shell(&cmd, vars)?;

            // Store for future bang expansion
            vars.last_shell_cmd = Some(cmd);

            println!("!");
            Ok(EscapeResult::continue_input())
        }
        '?' => {
            // Help
            print_escape_help();
            Ok(EscapeResult::continue_input())
        }
        ':' | '_' => {
            // Execute mailx command
            // In input mode, only a subset of commands are valid
            if !args.is_empty() {
                if let Err(e) = execute_input_mode_command(args, vars) {
                    eprintln!("{}", e);
                }
            }
            Ok(EscapeResult::continue_input())
        }
        'A' => {
            // Insert Sign variable
            if let Some(sign) = vars.get("Sign") {
                let sign = expand_escapes(sign);
                msg.body.push_str(&sign);
                msg.body.push('\n');
            }
            Ok(EscapeResult::continue_input())
        }
        'a' => {
            // Insert sign variable
            if let Some(sign) = vars.get("sign") {
                let sign = expand_escapes(sign);
                msg.body.push_str(&sign);
                msg.body.push('\n');
            }
            Ok(EscapeResult::continue_input())
        }
        'b' => {
            // Add Bcc recipients
            for addr in args.split_whitespace() {
                msg.add_bcc(addr);
            }
            Ok(EscapeResult::continue_input())
        }
        'c' => {
            // Add Cc recipients
            for addr in args.split_whitespace() {
                msg.add_cc(addr);
            }
            Ok(EscapeResult::continue_input())
        }
        'd' => {
            // Read dead letter file
            let dead_path = crate::util::dead_letter_path(vars);

            if let Ok(content) = fs::read_to_string(&dead_path) {
                msg.body.push_str(&content);
                println!("\"{}\" {} bytes", dead_path, content.len());
            } else {
                println!("{}: No such file", dead_path);
            }
            Ok(EscapeResult::continue_input())
        }
        'e' => {
            // Edit with EDITOR
            edit_message(msg, vars.get("EDITOR").unwrap_or("ed"))?;
            Ok(EscapeResult::continue_input())
        }
        'f' => {
            // Forward messages (without modification)
            if let Some(mailbox) = mb {
                forward_messages(args, msg, mailbox, vars, false)?;
            }
            Ok(EscapeResult::continue_input())
        }
        'F' => {
            // Forward messages (all headers)
            if let Some(mailbox) = mb {
                forward_messages(args, msg, mailbox, vars, true)?;
            }
            Ok(EscapeResult::continue_input())
        }
        'h' => {
            // Prompt for headers
            prompt_headers(msg)?;
            Ok(EscapeResult::continue_input())
        }
        'i' => {
            // Insert variable value
            if !args.is_empty() {
                if let Some(value) = vars.get(args) {
                    msg.body.push_str(value);
                    msg.body.push('\n');
                }
            }
            Ok(EscapeResult::continue_input())
        }
        'm' => {
            // Insert messages with indent
            if let Some(mailbox) = mb {
                insert_messages(args, msg, mailbox, vars, false, true)?;
            }
            Ok(EscapeResult::continue_input())
        }
        'M' => {
            // Insert messages with indent (all headers)
            if let Some(mailbox) = mb {
                insert_messages(args, msg, mailbox, vars, true, true)?;
            }
            Ok(EscapeResult::continue_input())
        }
        'p' => {
            // Print the message, using pager if longer than crt lines
            let mut output = String::from("-------\nMessage contains:\n");
            if !msg.to.is_empty() {
                output.push_str(&format!("To: {}\n", msg.to.join(", ")));
            }
            if !msg.cc.is_empty() {
                output.push_str(&format!("Cc: {}\n", msg.cc.join(", ")));
            }
            if !msg.bcc.is_empty() {
                output.push_str(&format!("Bcc: {}\n", msg.bcc.join(", ")));
            }
            if !msg.subject.is_empty() {
                output.push_str(&format!("Subject: {}\n", msg.subject));
            }
            output.push('\n');
            output.push_str(&msg.body);
            output.push_str("-------\n");

            crate::util::page_or_print(&output, vars);
            Ok(EscapeResult::continue_input())
        }
        'q' => {
            // Quit, save to dead letter
            Ok(EscapeResult::abort_with_save())
        }
        'r' | '<' => {
            // Read file or command output
            if let Some(cmd) = args.strip_prefix('!') {
                // Read command output
                let output = crate::util::shell_output(cmd, vars)?;
                msg.body.push_str(&output);
                println!("{} bytes", output.len());
            } else {
                // Read file
                let path = expand_filename(args, vars);
                match fs::read_to_string(&path) {
                    Ok(content) => {
                        msg.body.push_str(&content);
                        println!("\"{}\" {} bytes", path, content.len());
                    }
                    Err(e) => {
                        println!("{}: {}", path, e);
                    }
                }
            }
            Ok(EscapeResult::continue_input())
        }
        's' => {
            // Set subject
            msg.subject = args.to_string();
            Ok(EscapeResult::continue_input())
        }
        't' => {
            // Add To recipients
            for addr in args.split_whitespace() {
                msg.add_to(addr);
            }
            Ok(EscapeResult::continue_input())
        }
        'v' => {
            // Edit with VISUAL
            edit_message(msg, vars.get("VISUAL").unwrap_or("vi"))?;
            Ok(EscapeResult::continue_input())
        }
        'w' => {
            // Write body to file: create, or append if it already exists
            // (spec 105094-105097).
            let path = expand_filename(args, vars);
            let result = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
                .and_then(|mut f| f.write_all(msg.body.as_bytes()));
            match result {
                Ok(_) => println!("\"{}\" {} bytes", path, msg.body.len()),
                Err(e) => println!("{}: {}", path, e),
            }
            Ok(EscapeResult::continue_input())
        }
        'x' => {
            // Exit without saving
            Ok(EscapeResult::abort_without_save())
        }
        '|' => {
            // Pipe body through command
            if !args.is_empty() {
                match pipe_through_command(&msg.body, args, vars) {
                    Ok(output) => {
                        msg.body = output;
                    }
                    Err(e) => {
                        eprintln!("{}", e);
                    }
                }
            }
            Ok(EscapeResult::continue_input())
        }
        '~' => {
            // Literal escape character
            msg.body.push('~');
            msg.body.push_str(args);
            msg.body.push('\n');
            Ok(EscapeResult::continue_input())
        }
        _ => {
            // Unknown escape
            println!("Unknown escape: ~{}", cmd_char);
            Ok(EscapeResult::continue_input())
        }
    }
}

fn print_escape_help() {
    println!(
        r#"
    ~.      End message input
    ~!cmd   Execute shell command
    ~?      Print this help
    ~A      Insert Sign variable
    ~a      Insert sign variable
    ~b addr Add blind carbon copy
    ~c addr Add carbon copy
    ~d      Read dead letter file
    ~e      Edit message with EDITOR
    ~f msg  Forward messages
    ~F msg  Forward messages (all headers)
    ~h      Prompt for headers
    ~i var  Insert variable value
    ~m msg  Insert messages with indent
    ~M msg  Insert messages with indent (all headers)
    ~p      Print message
    ~q      Quit, save to dead letter
    ~r file Read file into message
    ~s subj Set subject
    ~t addr Add To recipient
    ~v      Edit message with VISUAL
    ~w file Write message body to file
    ~x      Exit without saving
    ~|cmd   Pipe message body through command
    ~~      Insert literal ~
"#
    );
}

fn expand_escapes(s: &str) -> String {
    s.replace("\\t", "\t").replace("\\n", "\n")
}

/// Resolve a filename named by a command escape.
///
/// A command escape names "the file named by the pathname file" (spec
/// 105085, 105094) -- unlike a command-mode filename, which POSIX does put
/// through shell word expansion (spec 104704-104711). The `+folder` prefix is
/// still honored, since that is mailx's own notation rather than the shell's.
///
/// This used to run the name through `sh -c "printf '%s' <name>"` unquoted, so
/// `~w my file.txt` wrote to `myfile.txt`, `~w $(cmd)out` executed `cmd`, and a
/// name the shell could not parse silently fell back to itself.
fn expand_filename(name: &str, vars: &Variables) -> String {
    let name = name.trim();

    // Handle + prefix (folder variable) - this is mailx-specific, not shell
    if let Some(rest) = name.strip_prefix('+') {
        if let Some(folder) = vars.get("folder") {
            let folder = if folder.starts_with('/') {
                folder.to_string()
            } else {
                let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
                format!("{}/{}", home, folder)
            };
            return format!("{}/{}", folder, rest);
        }
    }

    name.to_string()
}

fn edit_message(msg: &mut ComposedMessage, editor: &str) -> Result<(), String> {
    // The temporary comes from `mkstemp` (`O_EXCL`, mode 0600, honoring
    // `$TMPDIR`). The old `/tmp/mailx.<pid>` name was predictable and
    // world-readable, and writing to it followed a symlink planted in advance.
    let mut tmp = plib::tmp::NamedTempFile::new().map_err(|e| e.to_string())?;
    tmp.as_file_mut()
        .write_all(msg.format().as_bytes())
        .map_err(|e| e.to_string())?;
    tmp.as_file_mut().flush().map_err(|e| e.to_string())?;

    Command::new(editor)
        .arg(tmp.path())
        .status()
        .map_err(|e| e.to_string())?;

    let edited = fs::read_to_string(tmp.path()).map_err(|e| e.to_string())?;
    parse_edited_message(&edited, msg)?;

    println!("(continue)");
    Ok(())
}

fn parse_edited_message(content: &str, msg: &mut ComposedMessage) -> Result<(), String> {
    let mut in_headers = true;
    let mut body = String::new();

    msg.to.clear();
    msg.cc.clear();
    msg.bcc.clear();
    msg.subject.clear();
    msg.headers.clear();

    for line in content.lines() {
        if in_headers {
            if line.is_empty() {
                in_headers = false;
                continue;
            }

            if let Some(colon_pos) = line.find(':') {
                let name = line[..colon_pos].trim().to_lowercase();
                let value = line[colon_pos + 1..].trim();

                match name.as_str() {
                    "to" => {
                        for addr in value.split(',') {
                            msg.add_to(addr);
                        }
                    }
                    "cc" => {
                        for addr in value.split(',') {
                            msg.add_cc(addr);
                        }
                    }
                    "bcc" => {
                        for addr in value.split(',') {
                            msg.add_bcc(addr);
                        }
                    }
                    "subject" => {
                        msg.subject = value.to_string();
                    }
                    _ => {
                        msg.headers
                            .push((line[..colon_pos].trim().to_string(), value.to_string()));
                    }
                }
            }
        } else {
            body.push_str(line);
            body.push('\n');
        }
    }

    msg.body = body;
    Ok(())
}

/// `~h`: prompt for the Subject, To, Cc, and Bcc header fields.
///
/// Only when standard input is a terminal (spec 105065).
fn prompt_headers(msg: &mut ComposedMessage) -> Result<(), String> {
    if !io::stdin().is_terminal() {
        return Ok(());
    }

    if let Some(v) = crate::util::prompt_field("Subject", &msg.subject)? {
        msg.subject = v;
    }
    for (label, current, list) in [
        ("To", msg.to.join(", "), 0),
        ("Cc", msg.cc.join(", "), 1),
        ("Bcc", msg.bcc.join(", "), 2),
    ] {
        if let Some(v) = crate::util::prompt_field(label, &current)? {
            let target = match list {
                0 => &mut msg.to,
                1 => &mut msg.cc,
                _ => &mut msg.bcc,
            };
            target.clear();
            for addr in crate::util::addresses(&v) {
                if !target.contains(&addr.to_string()) {
                    target.push(addr.to_string());
                }
            }
        }
    }

    Ok(())
}

fn forward_messages(
    args: &str,
    msg: &mut ComposedMessage,
    mb: &Mailbox,
    vars: &Variables,
    all_headers: bool,
) -> Result<(), String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get(num) {
            let content = if all_headers {
                m.format_display(true, vars)
            } else {
                m.format_display(false, vars)
            };
            msg.body.push_str(&content);
        }
    }

    Ok(())
}

fn insert_messages(
    args: &str,
    msg: &mut ComposedMessage,
    mb: &Mailbox,
    vars: &Variables,
    all_headers: bool,
    with_indent: bool,
) -> Result<(), String> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    let indent = if with_indent {
        vars.get("indentprefix").unwrap_or("\t")
    } else {
        ""
    };

    for num in msg_nums {
        if let Some(m) = mb.get(num) {
            let content = if all_headers {
                m.format_display(true, vars)
            } else {
                m.format_display(false, vars)
            };

            for line in content.lines() {
                if !line.is_empty() {
                    msg.body.push_str(indent);
                }
                msg.body.push_str(line);
                msg.body.push('\n');
            }
        }
    }

    Ok(())
}

fn pipe_through_command(input: &str, cmd: &str, vars: &Variables) -> Result<String, String> {
    let shell = vars.get("SHELL").unwrap_or("/bin/sh");

    let mut child = Command::new(shell)
        .arg("-c")
        .arg("--")
        .arg(cmd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to execute command: {}", e))?;

    // Feed the body from a separate thread. Writing it all before reading the
    // filter's output deadlocked as soon as that output exceeded one pipe
    // buffer: the child blocked writing to a full stdout while we blocked
    // writing to a full stdin.
    let mut stdin = child.stdin.take().ok_or("Failed to open stdin")?;
    let body = input.to_string();
    let writer = std::thread::spawn(move || stdin.write_all(body.as_bytes()));

    let output = child
        .wait_with_output()
        .map_err(|e| format!("Failed to wait for command: {}", e))?;

    match writer.join() {
        Ok(Ok(())) => {}
        // A filter that exits without reading its input (`~| head`) closes the
        // pipe early; that is not an error in the message.
        Ok(Err(e)) if e.kind() == io::ErrorKind::BrokenPipe => {}
        Ok(Err(e)) => return Err(format!("Failed to write to command: {}", e)),
        Err(_) => return Err("Failed to write to command".to_string()),
    }

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!(
            "Command failed with exit code {}: {}",
            output.status.code().unwrap_or(-1),
            stderr
        ))
    }
}

/// Execute a mailx command-level request from input mode (`~:` / `~_`).
///
/// Per spec 105048-105049 this performs the command-level request. It goes
/// through the one interpreter, which decides what is legal here from each
/// command's context mask -- this used to be a third hand-written dispatcher
/// with its own copy of the argument parser and its own `!` handling that,
/// unlike the other two, did no bang expansion.
fn execute_input_mode_command(line: &str, vars: &mut Variables) -> Result<(), String> {
    // Input mode has no message store; commands needing one are excluded by
    // their context mask, so this scratch mailbox is never read from.
    let mut scratch = Mailbox::new(String::new());
    crate::commands::execute_in(line, &mut scratch, vars, crate::commands::Context::Input)
        .map(|_| ())
}
