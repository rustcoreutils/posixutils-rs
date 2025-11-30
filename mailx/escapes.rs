//! Tilde escape handling for mailx input mode

use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::process::{Command, Stdio};

use crate::mailbox::Mailbox;
use crate::msglist::parse_msglist;
use crate::send::ComposedMessage;
use crate::variables::Variables;

/// Result of handling an escape
pub struct EscapeResult {
    pub done: bool,
    pub abort: bool,
}

impl EscapeResult {
    pub fn continue_input() -> Self {
        EscapeResult {
            done: false,
            abort: false,
        }
    }

    pub fn finish() -> Self {
        EscapeResult {
            done: true,
            abort: false,
        }
    }

    pub fn abort() -> Self {
        EscapeResult {
            done: true,
            abort: true,
        }
    }
}

/// Handle an escape command
pub fn handle_escape(
    line: &str,
    msg: &mut ComposedMessage,
    vars: &Variables,
    mb: Option<&Mailbox>,
) -> Result<EscapeResult, String> {
    if line.is_empty() {
        // Just the escape char - treat as literal
        return Ok(EscapeResult::continue_input());
    }

    let cmd_char = line.chars().next().unwrap();
    let args = line[1..].trim();

    match cmd_char {
        '.' => {
            // End message input
            Ok(EscapeResult::finish())
        }
        '!' => {
            // Shell escape
            run_shell_command(args, vars)?;
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
                if let Err(e) = execute_input_mode_command(args, msg, mb, vars) {
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
            let dead_path = vars.get("DEAD").map(|s| s.to_string()).unwrap_or_else(|| {
                let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
                format!("{}/dead.letter", home)
            });

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
            // Print the message
            println!("-------\nMessage contains:");
            if !msg.to.is_empty() {
                println!("To: {}", msg.to.join(", "));
            }
            if !msg.cc.is_empty() {
                println!("Cc: {}", msg.cc.join(", "));
            }
            if !msg.bcc.is_empty() {
                println!("Bcc: {}", msg.bcc.join(", "));
            }
            if !msg.subject.is_empty() {
                println!("Subject: {}", msg.subject);
            }
            println!();
            print!("{}", msg.body);
            println!("-------");
            Ok(EscapeResult::continue_input())
        }
        'q' => {
            // Quit, save to dead letter
            Ok(EscapeResult::abort())
        }
        'r' | '<' => {
            // Read file or command output
            if let Some(cmd) = args.strip_prefix('!') {
                // Read command output
                let output = run_shell_command_output(cmd, vars)?;
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
            // Write body to file
            let path = expand_filename(args, vars);
            match fs::write(&path, &msg.body) {
                Ok(_) => println!("\"{}\" {} bytes", path, msg.body.len()),
                Err(e) => println!("{}: {}", path, e),
            }
            Ok(EscapeResult::continue_input())
        }
        'x' => {
            // Exit without saving
            Ok(EscapeResult {
                done: true,
                abort: true,
            })
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

fn run_shell_command(cmd: &str, vars: &Variables) -> Result<(), String> {
    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    Command::new(shell)
        .arg("-c")
        .arg(cmd)
        .status()
        .map_err(|e| e.to_string())?;
    Ok(())
}

fn run_shell_command_output(cmd: &str, vars: &Variables) -> Result<String, String> {
    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    let output = Command::new(shell)
        .arg("-c")
        .arg(cmd)
        .output()
        .map_err(|e| e.to_string())?;

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
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
            let expanded_rest = shell_expand(rest, vars);
            return format!("{}/{}", folder, expanded_rest);
        }
    }

    // Use shell for word expansion (POSIX wordexp semantics)
    shell_expand(name, vars)
}

/// Perform shell word expansion on a string using /bin/sh
/// This provides POSIX wordexp() semantics: tilde, parameter, command substitution, etc.
fn shell_expand(s: &str, vars: &Variables) -> String {
    if s.is_empty() {
        return s.to_string();
    }

    // Shell out to get proper expansion
    // Use printf %s to avoid issues with echo and backslashes
    let shell = vars.get("SHELL").unwrap_or("/bin/sh");

    let output = Command::new(shell)
        .arg("-c")
        .arg(format!("printf '%s' {}", s))
        .output();

    match output {
        Ok(out) if out.status.success() => String::from_utf8_lossy(&out.stdout).to_string(),
        _ => {
            // If shell expansion fails, return original string
            s.to_string()
        }
    }
}

fn edit_message(msg: &mut ComposedMessage, editor: &str) -> Result<(), String> {
    // Create temp file
    let temp_path = format!("/tmp/mailx.{}", std::process::id());

    // Write message to temp file
    let content = msg.format();
    fs::write(&temp_path, &content).map_err(|e| e.to_string())?;

    // Run editor
    Command::new(editor)
        .arg(&temp_path)
        .status()
        .map_err(|e| e.to_string())?;

    // Read back
    let edited = fs::read_to_string(&temp_path).map_err(|e| e.to_string())?;

    // Parse the edited content
    parse_edited_message(&edited, msg)?;

    // Clean up
    let _ = fs::remove_file(&temp_path);

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

fn prompt_headers(msg: &mut ComposedMessage) -> Result<(), String> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    // Subject
    print!("Subject: {}", msg.subject);
    stdout.flush().map_err(|e| e.to_string())?;
    let mut line = String::new();
    stdin
        .lock()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;
    if !line.trim().is_empty() {
        msg.subject = line.trim().to_string();
    }

    // To
    print!("To: {}", msg.to.join(", "));
    stdout.flush().map_err(|e| e.to_string())?;
    line.clear();
    stdin
        .lock()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;
    if !line.trim().is_empty() {
        msg.to.clear();
        for addr in line.split(',') {
            msg.add_to(addr);
        }
    }

    // Cc
    print!("Cc: {}", msg.cc.join(", "));
    stdout.flush().map_err(|e| e.to_string())?;
    line.clear();
    stdin
        .lock()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;
    if !line.trim().is_empty() {
        msg.cc.clear();
        for addr in line.split(',') {
            msg.add_cc(addr);
        }
    }

    // Bcc
    print!("Bcc: {}", msg.bcc.join(", "));
    stdout.flush().map_err(|e| e.to_string())?;
    line.clear();
    stdin
        .lock()
        .read_line(&mut line)
        .map_err(|e| e.to_string())?;
    if !line.trim().is_empty() {
        msg.bcc.clear();
        for addr in line.split(',') {
            msg.add_bcc(addr);
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
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    for num in msg_nums {
        if let Some(m) = mb.get(num) {
            let content = if all_headers {
                m.format_display(true, &[], &[])
            } else {
                m.format_display(false, &vars.ignored_headers, &vars.retained_headers)
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
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    let indent = if with_indent {
        vars.get("indentprefix").unwrap_or("\t")
    } else {
        ""
    };

    for num in msg_nums {
        if let Some(m) = mb.get(num) {
            let content = if all_headers {
                m.format_display(true, &[], &[])
            } else {
                m.format_display(false, &vars.ignored_headers, &vars.retained_headers)
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
        .arg(cmd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to execute command: {}", e))?;

    {
        let stdin = child.stdin.as_mut().ok_or("Failed to open stdin")?;
        stdin
            .write_all(input.as_bytes())
            .map_err(|e| format!("Failed to write to command: {}", e))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|e| format!("Failed to wait for command: {}", e))?;

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

/// Execute a mailx command from input mode (~: or ~_)
/// Only a subset of commands are allowed in input mode
fn execute_input_mode_command(
    line: &str,
    _msg: &mut ComposedMessage,
    _mb: Option<&Mailbox>,
    vars: &Variables,
) -> Result<(), String> {
    let line = line.trim();
    if line.is_empty() {
        return Ok(());
    }

    // Parse command and arguments
    let (cmd, args) = if let Some(pos) = line.find(char::is_whitespace) {
        (&line[..pos], line[pos..].trim())
    } else {
        (line, "")
    };

    let cmd_lower = cmd.to_lowercase();

    // Handle allowed commands in input mode
    match cmd_lower.as_str() {
        // set/unset variables
        "se" | "set" => {
            for arg in args.split_whitespace() {
                println!("{}", arg);
            }
            Ok(())
        }
        // echo
        "ec" | "ech" | "echo" => {
            println!("{}", args);
            Ok(())
        }
        // Shell escape
        cmd if cmd.starts_with('!') => {
            let shell_cmd = &line[1..];
            run_shell_command(shell_cmd, vars)?;
            println!("!");
            Ok(())
        }
        _ => Err(format!("Unknown command: {}", cmd)),
    }
}
