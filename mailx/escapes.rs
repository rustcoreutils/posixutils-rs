//
// Copyright (c) 2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Tilde escape handling for mailx input mode

use std::fs;
use std::io::{self, IsTerminal, Write};
use std::process::{Command, Stdio};

use crate::commands::{Context, Interp};
use crate::error::{Error, IoContext, Result};
use crate::filename::expand_local_prefixes;
use crate::mailbox::Mailbox;
use crate::msglist::msglist_or_current;
use crate::send::ComposedMessage;
use crate::util::address_words;
use crate::variables::Variables;

/// What input mode does after a command escape.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Escape {
    /// Go on reading the message.
    Continue,
    /// End the message, to be sent (`~.`).
    Finish,
    /// Abandon the message (`~q`, `~x`), saving it to dead.letter or not.
    Abort { save_dead_letter: bool },
}

/// Perform the command escape `line`, the text after the escape character.
pub fn handle_escape(
    line: &str,
    msg: &mut ComposedMessage,
    vars: &mut Variables,
    interp: &mut Interp,
    mb: Option<&Mailbox>,
) -> Result<Escape> {
    let Some(cmd_char) = line.chars().next() else {
        return Ok(Escape::Continue);
    };
    let args = line[cmd_char.len_utf8()..].trim();

    match cmd_char {
        '.' => return Ok(Escape::Finish),
        'q' => {
            return Ok(Escape::Abort {
                save_dead_letter: true,
            })
        }
        'x' => {
            return Ok(Escape::Abort {
                save_dead_letter: false,
            })
        }
        '!' => shell_escape(args, vars, interp)?,
        '?' => print_escape_help(),
        ':' | '_' if !args.is_empty() => {
            if let Err(e) = execute_input_mode_command(args, vars, interp) {
                eprintln!("{}", e);
            }
        }
        ':' | '_' => {}
        'A' => insert_signature(msg, vars, "Sign"),
        'a' => insert_signature(msg, vars, "sign"),
        'i' => insert_variable(msg, vars, args),
        'b' => address_words(args).into_iter().for_each(|a| msg.add_bcc(a)),
        'c' => address_words(args).into_iter().for_each(|a| msg.add_cc(a)),
        't' => address_words(args).into_iter().for_each(|a| msg.add_to(a)),
        's' => msg.subject = args.to_string(),
        'h' => prompt_headers(msg)?,
        'd' => read_dead_letter(msg, vars)?,
        'r' | '<' => read_into_body(msg, args, vars)?,
        'w' => write_body(msg, args, vars)?,
        'e' => edit_message(msg, vars.get_nonempty("EDITOR").unwrap_or("ed"))?,
        'v' => edit_message(msg, vars.get_nonempty("VISUAL").unwrap_or("vi"))?,
        'p' => print_draft(msg, vars),
        '|' if !args.is_empty() => match pipe_through_command(&msg.body, args, vars) {
            Ok(output) => msg.body = output,
            Err(e) => eprintln!("{}", e),
        },
        '|' => {}
        'f' | 'F' | 'm' | 'M' => {
            if let Some(mb) = mb {
                let all_headers = cmd_char.is_ascii_uppercase();
                if cmd_char.eq_ignore_ascii_case(&'f') {
                    forward_messages(args, msg, mb, vars, all_headers)?;
                } else {
                    insert_messages(args, msg, mb, vars, all_headers)?;
                }
            }
        }
        '~' => {
            msg.body.push(b'~');
            msg.body.extend_from_slice(args.as_bytes());
            msg.body.push(b'\n');
        }
        _ => return Err(format!("Unknown escape: ~{}", cmd_char).into()),
    }
    Ok(Escape::Continue)
}

/// `~!`: run a shell command, with `bang` expansion.
fn shell_escape(args: &str, vars: &Variables, interp: &mut Interp) -> Result<()> {
    let cmd = if vars.is_set("bang") {
        crate::util::expand_bang(args, interp.last_shell_cmd.as_deref())
    } else {
        args.to_string()
    };
    crate::util::shell(&cmd, vars)?;
    interp.last_shell_cmd = Some(cmd);
    println!("!");
    Ok(())
}

/// `~i`: append the value of variable `name` and a newline, unless it is
/// unset or null.
fn insert_variable(msg: &mut ComposedMessage, vars: &Variables, name: &str) {
    if let Some(value) = vars.get_nonempty(name) {
        msg.body.extend_from_slice(value.as_bytes());
        msg.body.push(b'\n');
    }
}

/// `~a`/`~A`: as `~i sign`/`~i Sign`, with `\t` and `\n` in the value
/// standing for a tab and a newline.
fn insert_signature(msg: &mut ComposedMessage, vars: &Variables, name: &str) {
    if let Some(sign) = vars.get_nonempty(name) {
        msg.body.extend_from_slice(expand_escapes(sign).as_bytes());
        msg.body.push(b'\n');
    }
}

/// `~d`: append the dead-letter file.
fn read_dead_letter(msg: &mut ComposedMessage, vars: &Variables) -> Result<()> {
    let path = crate::util::dead_letter_path(vars);
    let content = fs::read(&path).context(&path)?;
    msg.body.extend_from_slice(&content);
    println!("\"{}\" {} bytes", path, content.len());
    Ok(())
}

/// `~r`/`~<`: append a file, or with a leading `!` a command's output.
fn read_into_body(msg: &mut ComposedMessage, args: &str, vars: &Variables) -> Result<()> {
    if let Some(cmd) = args.strip_prefix('!') {
        let output = crate::util::shell_output_bytes(cmd, vars)?;
        msg.body.extend_from_slice(&output);
        println!("{} bytes", output.len());
        return Ok(());
    }
    let path = expand_local_prefixes(args, vars);
    let content = fs::read(&path).context(&path)?;
    msg.body.extend_from_slice(&content);
    println!("\"{}\" {} bytes", path, content.len());
    Ok(())
}

/// `~w`: write the body to a file, created or appended to (spec 105094-105097).
fn write_body(msg: &ComposedMessage, args: &str, vars: &Variables) -> Result<()> {
    let path = expand_local_prefixes(args, vars);
    crate::util::private_open()
        .create(true)
        .append(true)
        .open(&path)
        .and_then(|mut f| f.write_all(&msg.body))
        .context(&path)?;
    println!("\"{}\" {} bytes", path, msg.body.len());
    Ok(())
}

/// `~p`: write the message being composed, paginated by `crt`.
fn print_draft(msg: &ComposedMessage, vars: &Variables) {
    let mut output = b"-------\nMessage contains:\n".to_vec();
    for line in msg.header_lines(true) {
        output.extend_from_slice(line.as_bytes());
        output.push(b'\n');
    }
    output.push(b'\n');
    output.extend_from_slice(&msg.body);
    output.extend_from_slice(b"-------\n");
    crate::util::page_or_print(&output, vars);
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

fn edit_message(msg: &mut ComposedMessage, editor: &str) -> Result<()> {
    // The temporary comes from `mkstemp` (`O_EXCL`, mode 0600, honoring
    // `$TMPDIR`). The old `/tmp/mailx.<pid>` name was predictable and
    // world-readable, and writing to it followed a symlink planted in advance.
    let mut tmp = plib::tmp::NamedTempFile::new().map_err(|e| e.to_string())?;
    tmp.as_file_mut()
        .write_all(&msg.format())
        .map_err(|e| e.to_string())?;
    tmp.as_file_mut().flush().map_err(|e| e.to_string())?;

    Command::new(editor)
        .arg(tmp.path())
        .status()
        .map_err(|e| e.to_string())?;

    let edited = fs::read(tmp.path()).map_err(|e| e.to_string())?;
    parse_edited_message(&edited, msg)?;

    println!("(continue)");
    Ok(())
}

fn parse_edited_message(content: &[u8], msg: &mut ComposedMessage) -> Result<()> {
    let mut in_headers = true;
    let mut body = Vec::new();

    msg.to.clear();
    msg.cc.clear();
    msg.bcc.clear();
    msg.subject.clear();
    msg.headers.clear();

    for raw in crate::util::byte_lines(content) {
        if in_headers {
            if raw.is_empty() {
                in_headers = false;
                continue;
            }
            // Header fields are text; the body stays as the bytes edited.
            let line = String::from_utf8_lossy(raw);

            if let Some(colon_pos) = line.find(':') {
                let name = line[..colon_pos].trim().to_lowercase();
                let value = line[colon_pos + 1..].trim();

                match name.as_str() {
                    "to" => {
                        for addr in crate::util::addresses(value) {
                            msg.add_to(addr);
                        }
                    }
                    "cc" => {
                        for addr in crate::util::addresses(value) {
                            msg.add_cc(addr);
                        }
                    }
                    "bcc" => {
                        for addr in crate::util::addresses(value) {
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
            body.extend_from_slice(raw);
            body.push(b'\n');
        }
    }

    msg.body = body;
    Ok(())
}

/// `~h`: prompt for the Subject, To, Cc, and Bcc header fields.
///
/// Only when standard input is a terminal (spec 105065).
fn prompt_headers(msg: &mut ComposedMessage) -> Result<()> {
    if !io::stdin().is_terminal() {
        return Ok(());
    }
    if let Some(v) = crate::util::prompt_field("Subject", &msg.subject)? {
        msg.subject = v;
    }
    prompt_address_list("To", &mut msg.to)?;
    prompt_address_list("Cc", &mut msg.cc)?;
    prompt_address_list("Bcc", &mut msg.bcc)
}

/// Prompt with the addresses in `list`; a new answer replaces them.
fn prompt_address_list(label: &str, list: &mut Vec<String>) -> Result<()> {
    if let Some(answer) = crate::util::prompt_field(label, &list.join(", "))? {
        list.clear();
        for addr in crate::util::addresses(&answer) {
            crate::send::push_address(list, addr);
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
) -> Result<()> {
    let msg_nums = msglist_or_current(args, mb, vars)?;

    for num in msg_nums {
        if let Some(m) = mb.get(num) {
            msg.body
                .extend_from_slice(&m.format_display(all_headers, vars));
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
) -> Result<()> {
    let msg_nums = msglist_or_current(args, mb, vars)?;
    let indent = vars.get("indentprefix").unwrap_or("\t");

    for num in msg_nums {
        if let Some(m) = mb.get(num) {
            let content = m.format_display(all_headers, vars);
            for line in crate::util::byte_lines(&content) {
                if !line.is_empty() {
                    msg.body.extend_from_slice(indent.as_bytes());
                }
                msg.body.extend_from_slice(line);
                msg.body.push(b'\n');
            }
        }
    }

    Ok(())
}

fn pipe_through_command(input: &[u8], cmd: &str, vars: &Variables) -> Result<Vec<u8>> {
    let mut child = crate::util::shell_command(cmd, vars)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to execute command")?;

    // Feed the body from a separate thread. Writing it all before reading the
    // filter's output deadlocked as soon as that output exceeded one pipe
    // buffer: the child blocked writing to a full stdout while we blocked
    // writing to a full stdin.
    // Held until after the join below, for the `~| head` case the match on
    // `writer.join()` already anticipates.
    let _sigpipe = plib::io::SigPipeIgnored::new();

    let mut stdin = child.stdin.take().ok_or("Failed to open stdin")?;
    let body = input.to_vec();
    let writer = std::thread::spawn(move || stdin.write_all(&body));

    let output = child
        .wait_with_output()
        .context("Failed to wait for command")?;

    match writer.join() {
        Ok(Ok(())) => {}
        // A filter that exits without reading its input (`~| head`) closes the
        // pipe early; that is not an error in the message.
        Ok(Err(e)) if e.kind() == io::ErrorKind::BrokenPipe => {}
        Ok(Err(e)) => {
            return Err(Error::Io {
                context: "Failed to write to command".into(),
                source: e,
            })
        }
        Err(_) => return Err("Failed to write to command".into()),
    }

    if output.status.success() {
        Ok(output.stdout)
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!(
            "Command failed with exit code {}: {}",
            output.status.code().unwrap_or(-1),
            stderr
        )
        .into())
    }
}

/// Execute a mailx command-level request from input mode (`~:` / `~_`).
///
/// The one interpreter runs it (spec 105048-105049), and each command's
/// context mask decides whether it is legal in input mode.
fn execute_input_mode_command(line: &str, vars: &mut Variables, interp: &mut Interp) -> Result<()> {
    // Input mode has no message store; commands needing one are excluded by
    // their context mask, so this scratch mailbox is never read from.
    let mut scratch = Mailbox::new(String::new());
    crate::commands::execute_in(line, &mut scratch, vars, interp, Context::Input).map(|_| ())
}
