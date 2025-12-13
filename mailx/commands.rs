//! Command interpreter for mailx Receive Mode

use std::env;
use std::fs;
use std::io::{self, BufRead, IsTerminal, Write};
use std::process::Command;

use crate::escapes::handle_escape;
use crate::mailbox::Mailbox;
use crate::message::{extract_login, MessageState};
use crate::msglist::{parse_message, parse_msglist};
use crate::send::{compose_reply, send_message, ComposedMessage};
use crate::variables::{parse_set_arg, Variables};

/// Result of executing a command
pub enum CommandResult {
    Continue,
    Quit,
    Exit,
}

/// Execute a command in Receive Mode
pub fn execute_command(
    line: &str,
    mb: &mut Mailbox,
    vars: &mut Variables,
) -> Result<CommandResult, String> {
    let line = line.trim();

    // Check for blank line
    if line.is_empty() {
        return Ok(CommandResult::Continue);
    }

    // Check for comment
    if line.starts_with('#') {
        return Ok(CommandResult::Continue);
    }

    // Check for shell escape
    if let Some(cmd) = line.strip_prefix('!') {
        return cmd_shell(cmd, vars);
    }

    // Check for = (current message number)
    if line == "=" {
        println!("{}", mb.current);
        return Ok(CommandResult::Continue);
    }

    // Parse command and arguments
    let (cmd, args) = parse_command_line(line);

    // Match command
    match cmd.to_lowercase().as_str() {
        // No command means "next"
        "" => cmd_next(args, mb, vars),

        // Aliases
        "a" | "al" | "ali" | "alia" | "alias" => cmd_alias(args, vars),
        "g" | "gr" | "gro" | "grou" | "group" => cmd_alias(args, vars),

        // Alternates
        "alt" | "alte" | "alter" | "altern" | "alterna" | "alternat" | "alternate"
        | "alternates" => cmd_alternates(args, vars),

        // Change directory
        "cd" => cmd_cd(args),
        "ch" | "chd" | "chdi" | "chdir" => cmd_cd(args),

        // Copy
        "c" | "co" | "cop" | "copy" => cmd_copy(args, mb, vars, false),
        "C" | "Co" | "Cop" | "Copy" => cmd_copy_author(args, mb, vars),

        // Delete
        "d" | "de" | "del" | "dele" | "delet" | "delete" => cmd_delete(args, mb, vars),

        // Discard/Ignore headers
        "di" | "dis" | "disc" | "disca" | "discar" | "discard" => cmd_discard(args, vars),
        "ig" | "ign" | "igno" | "ignor" | "ignore" => cmd_discard(args, vars),

        // Delete and print
        "dp" => cmd_dp(args, mb, vars),
        "dt" => cmd_dp(args, mb, vars),

        // Echo
        "ec" | "ech" | "echo" => {
            println!("{}", args);
            Ok(CommandResult::Continue)
        }

        // Edit
        "e" | "ed" | "edi" | "edit" => cmd_edit(args, mb, vars),

        // Exit
        "ex" | "exi" | "exit" => Ok(CommandResult::Exit),
        "x" | "xi" | "xit" => Ok(CommandResult::Exit),

        // File/Folder
        "fi" | "fil" | "file" => cmd_file(args, mb, vars),
        "fold" | "folde" | "folder" => cmd_file(args, mb, vars),

        // Folders
        "folders" => cmd_folders(vars),

        // Followup
        "fo" | "fol" | "foll" | "follo" | "follow" | "followu" | "followup" => {
            cmd_followup(args, mb, vars, false)
        }
        "F" | "Fo" | "Fol" | "Foll" | "Follo" | "Follow" | "Followu" | "Followup" => {
            cmd_followup(args, mb, vars, true)
        }

        // From
        "f" | "fr" | "fro" | "from" => cmd_from(args, mb, vars),

        // Headers
        "h" | "he" | "hea" | "head" | "heade" | "header" | "headers" => cmd_headers(args, mb, vars),

        // Help
        "hel" | "help" | "?" => cmd_help(),

        // Hold/Preserve
        "ho" | "hol" | "hold" => cmd_hold(args, mb),
        "pre" | "pres" | "prese" | "preser" | "preserv" | "preserve" => cmd_hold(args, mb),

        // If/Else/Endif (for startup files)
        "i" | "if" => Ok(CommandResult::Continue), // Simplified - ignore conditionals
        "el" | "els" | "else" => Ok(CommandResult::Continue),
        "en" | "end" | "endi" | "endif" => Ok(CommandResult::Continue),

        // List
        "l" | "li" | "lis" | "list" => cmd_list(),

        // Mail
        "m" | "ma" | "mai" | "mail" => cmd_mail(args, mb, vars),

        // Mbox
        "mb" | "mbo" | "mbox" => cmd_mbox(args, mb),

        // Next
        "n" | "ne" | "nex" | "next" => cmd_next(args, mb, vars),

        // Pipe
        "pi" | "pip" | "pipe" | "|" => cmd_pipe(args, mb, vars),

        // Print/Type (with headers)
        "P" | "Pr" | "Pri" | "Prin" | "Print" => cmd_print(args, mb, vars, true),
        "T" | "Ty" | "Typ" | "Type" => cmd_print(args, mb, vars, true),

        // Print/Type
        "p" | "pr" | "pri" | "prin" | "print" => cmd_print(args, mb, vars, false),
        "t" | "ty" | "typ" | "type" => cmd_print(args, mb, vars, false),

        // Quit
        "q" | "qu" | "qui" | "quit" => Ok(CommandResult::Quit),

        // Reply (to sender only)
        "R" | "Re" | "Rep" | "Repl" | "Reply" | "Res" | "Resp" | "Respo" | "Respon" | "Respond" => {
            if vars.get_bool("flipr") {
                cmd_reply(args, mb, vars, true)
            } else {
                cmd_reply(args, mb, vars, false)
            }
        }

        // Reply (to all)
        "r" | "re" | "rep" | "repl" | "reply" | "res" | "resp" | "respo" | "respon" | "respond" => {
            if vars.get_bool("flipr") {
                cmd_reply(args, mb, vars, false)
            } else {
                cmd_reply(args, mb, vars, true)
            }
        }

        // Retain headers
        "ret" | "reta" | "retai" | "retain" => cmd_retain(args, vars),

        // Save
        "s" | "sa" | "sav" | "save" => cmd_save(args, mb, vars, true),
        "S" | "Sa" | "Sav" | "Save" => cmd_save_author(args, mb, vars),

        // Set
        "se" | "set" => cmd_set(args, vars),

        // Shell
        "sh" | "she" | "shel" | "shell" => cmd_shell_interactive(vars),

        // Size
        "si" | "siz" | "size" => cmd_size(args, mb),

        // Source
        "so" | "sou" | "sour" | "sourc" | "source" => cmd_source(args, vars),

        // Top
        "to" | "top" => cmd_top(args, mb, vars),

        // Touch
        "tou" | "touc" | "touch" => cmd_touch(args, mb),

        // Unalias
        "una" | "unal" | "unali" | "unalia" | "unalias" => cmd_unalias(args, vars),

        // Undelete
        "u" | "un" | "und" | "unde" | "undel" | "undele" | "undelet" | "undelete" => {
            cmd_undelete(args, mb, vars)
        }

        // Unset
        "uns" | "unse" | "unset" => cmd_unset(args, vars),

        // Visual
        "v" | "vi" | "vis" | "visu" | "visua" | "visual" => cmd_visual(args, mb, vars),

        // Write
        "w" | "wr" | "wri" | "writ" | "write" => cmd_write(args, mb, vars),

        // Scroll
        "z" => cmd_scroll(args, mb, vars),
        "z+" => cmd_scroll("+", mb, vars),
        "z-" => cmd_scroll("-", mb, vars),

        // Message number
        _ => {
            // Try to parse as a message number/list
            if let Ok(num) = cmd.parse::<usize>() {
                if num > 0 && num <= mb.message_count() {
                    mb.current = num;
                    cmd_print("", mb, vars, false)
                } else {
                    Err(format!("Invalid message number: {}", num))
                }
            } else {
                Err(format!("Unknown command: {}", cmd))
            }
        }
    }
}

/// Execute commands allowed in startup files
pub fn execute_startup_command(line: &str, vars: &mut Variables) -> Result<(), String> {
    let line = line.trim();

    if line.is_empty() || line.starts_with('#') {
        return Ok(());
    }

    let (cmd, args) = parse_command_line(line);

    match cmd.to_lowercase().as_str() {
        "a" | "al" | "ali" | "alia" | "alias" | "g" | "gr" | "gro" | "grou" | "group" => {
            cmd_alias_startup(args, vars)
        }
        "alt" | "alte" | "alter" | "altern" | "alterna" | "alternat" | "alternate"
        | "alternates" => cmd_alternates_startup(args, vars),
        "di" | "dis" | "disc" | "disca" | "discar" | "discard" | "ig" | "ign" | "igno"
        | "ignor" | "ignore" => cmd_discard_startup(args, vars),
        "ret" | "reta" | "retai" | "retain" => cmd_retain_startup(args, vars),
        "se" | "set" => cmd_set_startup(args, vars),
        "uns" | "unse" | "unset" => cmd_unset_startup(args, vars),
        "so" | "sou" | "sour" | "sourc" | "source" => cmd_source_startup(args, vars),
        "i" | "if" | "el" | "els" | "else" | "en" | "end" | "endi" | "endif" => Ok(()),
        _ => Ok(()), // Ignore unknown commands in startup
    }
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

fn cmd_alias(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
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

fn cmd_alias_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    if !args.is_empty() {
        let parts: Vec<&str> = args.split_whitespace().collect();
        if parts.len() > 1 {
            let name = parts[0].to_string();
            let addrs: Vec<String> = parts[1..].iter().map(|s| s.to_string()).collect();
            vars.aliases.insert(name, addrs);
        }
    }
    Ok(())
}

fn cmd_alternates(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
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

fn cmd_alternates_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    for name in args.split_whitespace() {
        if !vars.alternates.contains(&name.to_string()) {
            vars.alternates.push(name.to_string());
        }
    }
    Ok(())
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
    vars: &Variables,
    mark_saved: bool,
) -> Result<CommandResult, String> {
    // Parse arguments: [msglist] file
    let parts: Vec<&str> = args.split_whitespace().collect();

    if parts.is_empty() {
        return Err("No file specified".to_string());
    }

    let (msg_nums, filename) = if parts.len() == 1 {
        (vec![mb.current], expand_filename(parts[0], vars))
    } else {
        let file = parts.last().unwrap();
        let msglist = parts[..parts.len() - 1].join(" ");
        let nums = parse_msglist(&msglist, mb, false)?;
        (nums, expand_filename(file, vars))
    };

    mb.save_messages(&msg_nums, &filename, true)?;

    if mark_saved {
        for num in &msg_nums {
            if let Some(m) = mb.get_mut(*num) {
                m.state = MessageState::Saved;
            }
        }
    }

    let total_size: usize = msg_nums
        .iter()
        .filter_map(|&n| mb.get(n))
        .map(|m| m.size())
        .sum();
    println!(
        "\"{}\" {} messages {} bytes",
        filename,
        msg_nums.len(),
        total_size
    );

    Ok(CommandResult::Continue)
}

fn cmd_copy_author(
    args: &str,
    mb: &mut Mailbox,
    vars: &Variables,
) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    if let Some(first_msg) = msg_nums.first().and_then(|&n| mb.get(n)) {
        let filename = extract_login(first_msg.from());
        let filename = expand_filename(filename, vars);

        mb.save_messages(&msg_nums, &filename, true)?;

        let total_size: usize = msg_nums
            .iter()
            .filter_map(|&n| mb.get(n))
            .map(|m| m.size())
            .sum();
        println!(
            "\"{}\" {} messages {} bytes",
            filename,
            msg_nums.len(),
            total_size
        );
    }

    Ok(CommandResult::Continue)
}

fn cmd_delete(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    let max_deleted = msg_nums.iter().copied().max().unwrap_or(0);

    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.state = MessageState::Deleted;
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
            .map(|m| m.state != MessageState::Deleted)
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

fn cmd_discard_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    for header in args.split_whitespace() {
        if !vars.ignored_headers.contains(&header.to_string()) {
            vars.ignored_headers.push(header.to_string());
        }
    }
    Ok(())
}

fn cmd_dp(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    // Delete and print next
    cmd_delete(args, mb, vars)?;

    // Print the new current message
    if mb.current > 0
        && mb
            .get(mb.current)
            .map(|m| m.state != MessageState::Deleted)
            .unwrap_or(false)
    {
        cmd_print("", mb, vars, false)?;
    } else {
        println!("At EOF");
    }

    Ok(CommandResult::Continue)
}

fn cmd_edit(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    let editor = vars.get("EDITOR").unwrap_or("ed");

    for num in msg_nums {
        if let Some(msg) = mb.get(num) {
            let temp_path = format!("/tmp/mailx.{}.{}", std::process::id(), num);
            fs::write(&temp_path, msg.format_full()).map_err(|e| e.to_string())?;

            Command::new(editor)
                .arg(&temp_path)
                .status()
                .map_err(|e| e.to_string())?;

            let _ = fs::remove_file(&temp_path);
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

    // Save current mailbox first
    mb.quit(vars)?;

    // Open new mailbox
    let path = expand_folder_name(args, vars);
    let new_mb = Mailbox::load(&path).map_err(|e| format!("{}: {}", path, e))?;

    *mb = new_mb;
    mb.print_headers(None, vars);

    Ok(CommandResult::Continue)
}

fn cmd_folders(vars: &Variables) -> Result<CommandResult, String> {
    if let Some(folder) = vars.get("folder") {
        let folder = expand_filename(folder, vars);
        let lister = vars.get("LISTER").unwrap_or("ls");
        let shell = vars.get("SHELL").unwrap_or("/bin/sh");

        Command::new(shell)
            .arg("-c")
            .arg(format!("{} {}", lister, folder))
            .status()
            .map_err(|e| e.to_string())?;
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
        parse_message(args, mb)?
    };

    let original = mb.get(msg_num).ok_or("No message")?;
    let mut composed = compose_reply(original, reply_to_all, vars);

    // Record to file named after first recipient
    let record_file = extract_login(original.from());
    let record_file = expand_filename(record_file, vars);

    // Enter input mode
    compose_message(&mut composed, mb, vars)?;

    // Send and record
    send_message(&composed, vars, false)?;
    record_to_file(&composed, &record_file)?;

    if let Some(m) = mb.get_mut(msg_num) {
        m.state = MessageState::Read;
    }

    Ok(CommandResult::Continue)
}

fn cmd_from(args: &str, mb: &Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    mb.print_headers(Some(&msg_nums), vars);
    Ok(CommandResult::Continue)
}

fn cmd_headers(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    if !args.is_empty() {
        let msg_num = parse_message(args, mb)?;
        mb.current = msg_num;
    }

    let screen = vars.get_number("screen").unwrap_or(20) as usize;
    let start = if mb.current > 0 {
        ((mb.current - 1) / screen) * screen + 1
    } else {
        1
    };

    let nums: Vec<usize> = (start..=mb.message_count().min(start + screen - 1)).collect();
    mb.print_headers(Some(&nums), vars);

    Ok(CommandResult::Continue)
}

fn cmd_help() -> Result<CommandResult, String> {
    println!(
        r#"
Commands:
  ?           help
  =           print current message number
  alias       define/list aliases
  alternates  define alternate addresses
  cd          change directory
  copy        copy messages to file
  delete      delete messages
  dp          delete and print next
  edit        edit messages
  exit        exit without saving
  file        change mailbox
  folders     list folder directory
  from        print header summary
  headers     print header page
  hold        hold messages in mailbox
  list        list available commands
  mail        send mail
  mbox        mark for moving to mbox
  next        print next message
  pipe        pipe messages to command
  print       print messages
  quit        quit, saving changes
  reply       reply to message
  retain      retain headers
  save        save messages to file
  set         set/print variables
  shell       invoke shell
  size        print message sizes
  source      read commands from file
  top         print top lines of messages
  touch       mark messages for mbox
  undelete    undelete messages
  unset       unset variables
  visual      edit with visual editor
  write       write messages to file (no headers)
  z           scroll header display
"#
    );
    Ok(CommandResult::Continue)
}

fn cmd_hold(args: &str, mb: &mut Mailbox) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.state = MessageState::Preserved;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_list() -> Result<CommandResult, String> {
    println!(
        "alias alternates cd chdir copy Copy delete discard dp dt \
         echo edit exit file folder folders followup Followup from \
         headers help hold if else endif ignore list mail mbox next \
         pipe Print print quit Reply reply retain save Save set shell \
         size source top touch Type type unalias undelete unset visual \
         write z"
    );
    Ok(CommandResult::Continue)
}

fn cmd_mail(args: &str, mb: &Mailbox, vars: &mut Variables) -> Result<CommandResult, String> {
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

    // Prompt for subject if needed
    if io::stdin().is_terminal() && vars.get_bool("asksub") {
        print!("Subject: ");
        io::stdout().flush().map_err(|e| e.to_string())?;

        let mut subject = String::new();
        io::stdin()
            .read_line(&mut subject)
            .map_err(|e| e.to_string())?;
        composed.subject = subject.trim().to_string();
    }

    compose_message(&mut composed, mb, vars)?;
    send_message(&composed, vars, false)?;

    Ok(CommandResult::Continue)
}

fn cmd_mbox(args: &str, mb: &mut Mailbox) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.state = MessageState::Read;
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_next(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    if !args.is_empty() {
        let msg_num = parse_message(args, mb)?;
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

fn cmd_pipe(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
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
            if parse_msglist(&potential_msglist, mb, false).is_ok() {
                msglist_end = i + 1;
            }
        }

        if msglist_end == 0 || msglist_end >= words.len() {
            // All command or all msglist
            if let Ok(nums) = parse_msglist(args, mb, false) {
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
            let nums = parse_msglist(&msglist, mb, false)?;
            (nums, cmd)
        }
    };

    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    let page = vars.get_bool("page");

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            let mut child = std::process::Command::new(shell)
                .arg("-c")
                .arg(&cmd)
                .stdin(std::process::Stdio::piped())
                .spawn()
                .map_err(|e| e.to_string())?;

            if let Some(stdin) = child.stdin.as_mut() {
                let content =
                    msg.format_display(false, &vars.ignored_headers, &vars.retained_headers);
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

    // Mark as read
    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.state = MessageState::Read;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.current = last;
    }

    Ok(CommandResult::Continue)
}

fn cmd_print(
    args: &str,
    mb: &mut Mailbox,
    vars: &Variables,
    show_all_headers: bool,
) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    let crt = vars.get_number("crt");
    let pager = vars.get("PAGER").unwrap_or("more");

    for num in &msg_nums {
        if let Some(msg) = mb.get(*num) {
            let content = if show_all_headers {
                msg.format_display(true, &[], &[])
            } else {
                msg.format_display(false, &vars.ignored_headers, &vars.retained_headers)
            };

            // Check if we need to page
            if let Some(crt_lines) = crt {
                if content.lines().count() > crt_lines as usize {
                    // Use pager
                    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
                    let mut child = std::process::Command::new(shell)
                        .arg("-c")
                        .arg(pager)
                        .stdin(std::process::Stdio::piped())
                        .spawn()
                        .map_err(|e| e.to_string())?;

                    if let Some(stdin) = child.stdin.as_mut() {
                        stdin
                            .write_all(content.as_bytes())
                            .map_err(|e| e.to_string())?;
                    }

                    child.wait().map_err(|e| e.to_string())?;
                } else {
                    print!("{}", content);
                }
            } else {
                print!("{}", content);
            }
        }
    }

    // Mark as read and update current
    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.state = MessageState::Read;
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
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    for &num in &msg_nums {
        let original = mb.get(num).ok_or("No message")?;
        let mut composed = compose_reply(original, reply_to_all, vars);

        compose_message(&mut composed, mb, vars)?;
        send_message(&composed, vars, false)?;
    }

    // Mark as read
    for &num in &msg_nums {
        if let Some(m) = mb.get_mut(num) {
            m.state = MessageState::Read;
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

fn cmd_retain_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    for header in args.split_whitespace() {
        if !vars.retained_headers.contains(&header.to_string()) {
            vars.retained_headers.push(header.to_string());
        }
    }
    Ok(())
}

fn cmd_save(
    args: &str,
    mb: &mut Mailbox,
    vars: &Variables,
    mark_saved: bool,
) -> Result<CommandResult, String> {
    // Parse arguments: [msglist] [file]
    let parts: Vec<&str> = args.split_whitespace().collect();

    let (msg_nums, filename) = if parts.is_empty() {
        // Save current to mbox
        let mbox = vars.get("MBOX").map(|s| s.to_string()).unwrap_or_else(|| {
            let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
            format!("{}/mbox", home)
        });
        (vec![mb.current], mbox)
    } else if parts.len() == 1 {
        // Could be msglist or file
        if let Ok(nums) = parse_msglist(parts[0], mb, false) {
            // It's a msglist, save to mbox
            let mbox = vars.get("MBOX").map(|s| s.to_string()).unwrap_or_else(|| {
                let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
                format!("{}/mbox", home)
            });
            (nums, mbox)
        } else {
            // It's a file
            (vec![mb.current], expand_filename(parts[0], vars))
        }
    } else {
        let file = parts.last().unwrap();
        let msglist = parts[..parts.len() - 1].join(" ");
        let nums = parse_msglist(&msglist, mb, false)?;
        (nums, expand_filename(file, vars))
    };

    mb.save_messages(&msg_nums, &filename, true)?;

    if mark_saved {
        for num in &msg_nums {
            if let Some(m) = mb.get_mut(*num) {
                m.state = MessageState::Saved;
            }
        }
        mb.modified = true;
    }

    let total_size: usize = msg_nums
        .iter()
        .filter_map(|&n| mb.get(n))
        .map(|m| m.size())
        .sum();
    println!(
        "\"{}\" {} messages {} bytes",
        filename,
        msg_nums.len(),
        total_size
    );

    Ok(CommandResult::Continue)
}

fn cmd_save_author(
    args: &str,
    mb: &mut Mailbox,
    vars: &Variables,
) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    if let Some(first_msg) = msg_nums.first().and_then(|&n| mb.get(n)) {
        let filename = extract_login(first_msg.from());
        let filename = expand_filename(filename, vars);

        mb.save_messages(&msg_nums, &filename, true)?;

        for num in &msg_nums {
            if let Some(m) = mb.get_mut(*num) {
                m.state = MessageState::Saved;
            }
        }
        mb.modified = true;

        let total_size: usize = msg_nums
            .iter()
            .filter_map(|&n| mb.get(n))
            .map(|m| m.size())
            .sum();
        println!(
            "\"{}\" {} messages {} bytes",
            filename,
            msg_nums.len(),
            total_size
        );
    }

    Ok(CommandResult::Continue)
}

fn cmd_set(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    if args.is_empty() {
        vars.print_all();
        return Ok(CommandResult::Continue);
    }

    for arg in args.split_whitespace() {
        if arg.starts_with("no") && !arg.contains('=') {
            vars.unset(&arg[2..]);
        } else {
            let (name, value) = parse_set_arg(arg);
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
    }

    Ok(CommandResult::Continue)
}

fn cmd_set_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    for arg in args.split_whitespace() {
        if arg.starts_with("no") && !arg.contains('=') {
            vars.unset(&arg[2..]);
        } else {
            let (name, value) = parse_set_arg(arg);
            // Silently skip onehop - we permanently operate in noonehop mode
            if name == "onehop" {
                continue;
            }
            if let Some(val) = value {
                vars.set(name, val);
            } else {
                vars.set_bool(name, true);
            }
        }
    }
    Ok(())
}

fn cmd_shell(cmd: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    // Expand ! to previous command if bang is set
    let cmd = if vars.get_bool("bang") {
        expand_bang(cmd, vars.last_shell_cmd.as_deref())
    } else {
        cmd.to_string()
    };

    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    Command::new(shell)
        .arg("-c")
        .arg(&cmd)
        .status()
        .map_err(|e| e.to_string())?;

    // Save as last command
    vars.last_shell_cmd = Some(cmd);

    println!("!");
    Ok(CommandResult::Continue)
}

/// Expand unescaped ! to the previous command
fn expand_bang(cmd: &str, prev: Option<&str>) -> String {
    let prev = prev.unwrap_or("");
    let mut result = String::new();
    let mut chars = cmd.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Escaped character
            if let Some(&next) = chars.peek() {
                if next == '!' {
                    result.push('!');
                    chars.next();
                    continue;
                }
            }
            result.push(c);
        } else if c == '!' {
            // Expand to previous command
            result.push_str(prev);
        } else {
            result.push(c);
        }
    }

    result
}

fn cmd_shell_interactive(vars: &Variables) -> Result<CommandResult, String> {
    let shell = vars.get("SHELL").unwrap_or("/bin/sh");
    Command::new(shell).status().map_err(|e| e.to_string())?;
    Ok(CommandResult::Continue)
}

fn cmd_size(args: &str, mb: &Mailbox) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    for num in msg_nums {
        if let Some(msg) = mb.get(num) {
            println!("{}: {} bytes", num, msg.size());
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_source(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    let path = expand_filename(args.trim(), vars);
    let content = fs::read_to_string(&path).map_err(|e| format!("{}: {}", path, e))?;

    for line in content.lines() {
        let line = line.trim();
        if !line.is_empty() && !line.starts_with('#') {
            if let Err(e) = execute_startup_command(line, vars) {
                eprintln!("{}: {}", path, e);
            }
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_source_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    let path = expand_filename(args.trim(), vars);
    if let Ok(content) = fs::read_to_string(&path) {
        for line in content.lines() {
            let line = line.trim();
            if !line.is_empty() && !line.starts_with('#') {
                execute_startup_command(line, vars)?;
            }
        }
    }
    Ok(())
}

fn cmd_top(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

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

    // Mark as read
    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.state = MessageState::Read;
        }
    }
    if let Some(&last) = msg_nums.last() {
        mb.current = last;
    }

    Ok(CommandResult::Continue)
}

fn cmd_touch(args: &str, mb: &mut Mailbox) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    for num in msg_nums {
        if let Some(m) = mb.get_mut(num) {
            if m.state == MessageState::New || m.state == MessageState::Unread {
                m.state = MessageState::Read;
            }
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

fn cmd_undelete(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
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
        parse_msglist(args, mb, true)?
    };

    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            if m.state == MessageState::Deleted {
                m.state = MessageState::Read;
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

fn cmd_unset(args: &str, vars: &mut Variables) -> Result<CommandResult, String> {
    for name in args.split_whitespace() {
        vars.unset(name);
    }
    Ok(CommandResult::Continue)
}

fn cmd_unset_startup(args: &str, vars: &mut Variables) -> Result<(), String> {
    for name in args.split_whitespace() {
        vars.unset(name);
    }
    Ok(())
}

fn cmd_visual(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    let msg_nums = if args.is_empty() {
        vec![mb.current]
    } else {
        parse_msglist(args, mb, false)?
    };

    let editor = vars.get("VISUAL").unwrap_or("vi");

    for num in msg_nums {
        if let Some(msg) = mb.get(num) {
            let temp_path = format!("/tmp/mailx.{}.{}", std::process::id(), num);
            fs::write(&temp_path, msg.format_full()).map_err(|e| e.to_string())?;

            Command::new(editor)
                .arg(&temp_path)
                .status()
                .map_err(|e| e.to_string())?;

            let _ = fs::remove_file(&temp_path);
        }
    }

    Ok(CommandResult::Continue)
}

fn cmd_write(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    // Parse arguments: [msglist] file
    let parts: Vec<&str> = args.split_whitespace().collect();

    if parts.is_empty() {
        return Err("No file specified".to_string());
    }

    let (msg_nums, filename) = if parts.len() == 1 {
        (vec![mb.current], expand_filename(parts[0], vars))
    } else {
        let file = parts.last().unwrap();
        let msglist = parts[..parts.len() - 1].join(" ");
        let nums = parse_msglist(&msglist, mb, false)?;
        (nums, expand_filename(file, vars))
    };

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

    for num in &msg_nums {
        if let Some(m) = mb.get_mut(*num) {
            m.state = MessageState::Saved;
        }
    }
    mb.modified = true;

    println!(
        "\"{}\" {} messages {} bytes",
        filename,
        msg_nums.len(),
        total_size
    );

    Ok(CommandResult::Continue)
}

fn cmd_scroll(args: &str, mb: &mut Mailbox, vars: &Variables) -> Result<CommandResult, String> {
    let screen = vars.get_number("screen").unwrap_or(20) as usize;
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

fn expand_filename(name: &str, vars: &Variables) -> String {
    let name = name.trim();

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

    if let Some(rest) = name.strip_prefix('~') {
        let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
        return format!("{}{}", home, rest);
    }

    name.to_string()
}

fn expand_folder_name(name: &str, vars: &Variables) -> String {
    match name {
        "%" => {
            let user = env::var("USER").unwrap_or_else(|_| "unknown".to_string());
            format!("/var/mail/{}", user)
        }
        "&" => vars.get("MBOX").map(|s| s.to_string()).unwrap_or_else(|| {
            let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
            format!("{}/mbox", home)
        }),
        "#" => {
            // Previous file - not implemented
            name.to_string()
        }
        _ => {
            if let Some(user) = name.strip_prefix('%') {
                format!("/var/mail/{}", user)
            } else {
                expand_filename(name, vars)
            }
        }
    }
}

fn compose_message(
    composed: &mut ComposedMessage,
    mb: &Mailbox,
    vars: &mut Variables,
) -> Result<(), String> {
    let stdin = io::stdin();
    let escape_char = vars.escape_char();

    loop {
        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {}
            Err(e) => return Err(e.to_string()),
        }

        // Check for escape character
        if line.starts_with(escape_char) && line.len() > 1 {
            let result = handle_escape(&line[1..], composed, vars, Some(mb))?;
            if result.done {
                if result.abort {
                    return Err("Aborted".to_string());
                }
                break;
            }
            continue;
        }

        // Check for single period (if dot is set)
        if vars.get_bool("dot") && line.trim() == "." {
            break;
        }

        composed.body.push_str(&line);
    }

    Ok(())
}

fn record_to_file(msg: &ComposedMessage, path: &str) -> Result<(), String> {
    let user = env::var("USER").unwrap_or_else(|_| "unknown".to_string());
    let date = chrono::Local::now().format("%a %b %e %H:%M:%S %Y");
    let from_line = format!("From {} {}", user, date);

    let mut file = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .map_err(|e| format!("{}: {}", path, e))?;

    writeln!(file, "{}", from_line).map_err(|e| e.to_string())?;
    write!(file, "{}", msg.format()).map_err(|e| e.to_string())?;
    if !msg.body.ends_with('\n') {
        writeln!(file).map_err(|e| e.to_string())?;
    }
    writeln!(file).map_err(|e| e.to_string())?;

    Ok(())
}
