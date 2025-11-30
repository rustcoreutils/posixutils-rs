mod args;
mod commands;
mod escapes;
mod mailbox;
mod message;
mod msglist;
mod send;
mod variables;

use std::env;
use std::io::{self, BufRead, IsTerminal, Write};
use std::process;

use args::{Args, Mode};
use commands::execute_command;
use mailbox::Mailbox;
use send::send_mode;
use variables::Variables;

fn main() {
    let args = match Args::parse(env::args().skip(1).collect()) {
        Ok(args) => args,
        Err(e) => {
            eprintln!("mailx: {}", e);
            process::exit(1);
        }
    };

    let exit_code = match args.mode {
        Mode::Send => run_send_mode(&args),
        Mode::CheckMail => run_check_mail(&args),
        Mode::HeadersOnly => run_headers_only(&args),
        Mode::Receive => run_receive_mode(&args),
    };

    process::exit(exit_code);
}

fn run_send_mode(args: &Args) -> i32 {
    let mut vars = Variables::new();
    init_variables(&mut vars, args);

    // Load startup files unless -n is specified
    if !args.no_init {
        load_startup_files(&mut vars, RcMode::Send);
    }

    if let Err(e) = send_mode(args, &vars) {
        eprintln!("mailx: {}", e);
        return 1;
    }
    0
}

fn run_check_mail(args: &Args) -> i32 {
    let mailbox_path = args.file.clone().unwrap_or_else(get_system_mailbox);
    match Mailbox::load(&mailbox_path) {
        Ok(mb) if mb.message_count() > 0 => 0,
        Ok(_) => 1,
        Err(_) => 1,
    }
}

fn run_headers_only(args: &Args) -> i32 {
    let mut vars = Variables::new();
    init_variables(&mut vars, args);

    // Load startup files unless -n is specified
    if !args.no_init {
        load_startup_files(&mut vars, RcMode::Receive);
    }

    let mailbox_path = args.file.clone().unwrap_or_else(get_system_mailbox);
    let mb = match Mailbox::load(&mailbox_path) {
        Ok(mb) => mb,
        Err(e) => {
            eprintln!("mailx: {}: {}", mailbox_path, e);
            return 1;
        }
    };

    if mb.message_count() == 0 {
        println!("No mail for {}", get_user());
        return 0;
    }

    mb.print_headers(None, &vars);
    0
}

fn run_receive_mode(args: &Args) -> i32 {
    let mut vars = Variables::new();
    init_variables(&mut vars, args);

    // Load startup files unless -n is specified
    if !args.no_init {
        load_startup_files(&mut vars, RcMode::Receive);
    }

    let mailbox_path = if args.read_mbox {
        args.file.clone().unwrap_or_else(|| get_mbox_path())
    } else {
        args.file.clone().unwrap_or_else(get_system_mailbox)
    };

    let mut mb = match Mailbox::load(&mailbox_path) {
        Ok(mb) => mb,
        Err(e) => {
            if args.file.is_some() || !args.read_mbox {
                eprintln!("mailx: {}: {}", mailbox_path, e);
                return 1;
            }
            // For mbox, create empty if doesn't exist
            Mailbox::new(mailbox_path.clone())
        }
    };

    mb.set_is_system_mailbox(args.file.is_none() && !args.read_mbox);

    let is_tty = io::stdin().is_terminal();

    // Print version unless quiet
    if !vars.get_bool("quiet") && is_tty {
        println!("mailx-rs version {}", env!("CARGO_PKG_VERSION"));
    }

    // Print headers unless -N is specified
    if !args.no_header_summary && vars.get_bool("header") && mb.message_count() > 0 {
        mb.print_headers(None, &vars);
    } else if mb.message_count() == 0 && is_tty {
        println!("No mail for {}", get_user());
    }

    // Command loop
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        // Print prompt
        if is_tty {
            if let Some(prompt) = vars.get("prompt") {
                print!("{}", prompt);
                let _ = stdout.flush();
            }
        }

        // Read command
        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                // EOF - quit
                return quit_mailbox(&mut mb, &vars);
            }
            Ok(_) => {}
            Err(e) => {
                eprintln!("mailx: read error: {}", e);
                return 1;
            }
        }

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Execute command
        match execute_command(line, &mut mb, &mut vars) {
            Ok(commands::CommandResult::Continue) => {}
            Ok(commands::CommandResult::Quit) => {
                return quit_mailbox(&mut mb, &vars);
            }
            Ok(commands::CommandResult::Exit) => {
                return 0;
            }
            Err(e) => {
                eprintln!("{}", e);
            }
        }
    }
}

fn quit_mailbox(mb: &mut Mailbox, vars: &Variables) -> i32 {
    if let Err(e) = mb.quit(vars) {
        eprintln!("mailx: {}", e);
        return 1;
    }
    0
}

fn init_variables(vars: &mut Variables, args: &Args) {
    // Import environment variables
    if let Ok(val) = env::var("DEAD") {
        vars.set("DEAD", &val);
    }
    if let Ok(val) = env::var("EDITOR") {
        vars.set("EDITOR", &val);
    }
    if let Ok(val) = env::var("MBOX") {
        vars.set("MBOX", &val);
    }
    if let Ok(val) = env::var("LISTER") {
        vars.set("LISTER", &val);
    }
    if let Ok(val) = env::var("PAGER") {
        vars.set("PAGER", &val);
    }
    if let Ok(val) = env::var("SHELL") {
        vars.set("SHELL", &val);
    }
    if let Ok(val) = env::var("VISUAL") {
        vars.set("VISUAL", &val);
    }

    // Set ignore variable from -i option
    if args.ignore_interrupts {
        vars.set_bool("ignore", true);
    }
}

/// System startup file locations (checked in order)
const SYSTEM_MAILRC_PATHS: &[&str] = &[
    "/etc/mail.rc",
    "/etc/mailrc",
    "/usr/share/mailx/mailx.rc",
    "/usr/lib/mailx/mailx.rc",
];

fn load_startup_files(vars: &mut Variables, mode: RcMode) {
    // Load system startup file first (POSIX requirement)
    for path in SYSTEM_MAILRC_PATHS {
        if let Ok(content) = std::fs::read_to_string(path) {
            load_rc_content(&content, path, vars, mode);
            break; // Only load the first one found
        }
    }

    // Load user startup file
    let mailrc = env::var("MAILRC").unwrap_or_else(|_| {
        let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/.mailrc", home)
    });

    if let Ok(content) = std::fs::read_to_string(&mailrc) {
        load_rc_content(&content, &mailrc, vars, mode);
    }
}

/// Mode for conditional execution in startup files
#[derive(Clone, Copy, PartialEq)]
enum RcMode {
    Send,
    Receive,
}

fn load_rc_content(content: &str, path: &str, vars: &mut Variables, mode: RcMode) {
    // Stack to track conditional state: (condition_matches, in_else_branch)
    let mut cond_stack: Vec<(bool, bool)> = Vec::new();

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Parse the command
        let (cmd, args) = if let Some(pos) = line.find(char::is_whitespace) {
            (&line[..pos], line[pos..].trim())
        } else {
            (line, "")
        };
        let cmd_lower = cmd.to_lowercase();

        // Handle if/else/endif
        match cmd_lower.as_str() {
            "i" | "if" => {
                // `if s` for send mode, `if r` for receive mode
                let arg = args.trim().to_lowercase();
                let condition_matches = match arg.as_str() {
                    "s" => mode == RcMode::Send,
                    "r" => mode == RcMode::Receive,
                    _ => false, // Unknown condition, treat as false
                };
                cond_stack.push((condition_matches, false));
                continue;
            }
            "el" | "els" | "else" => {
                if let Some((matches, in_else)) = cond_stack.pop() {
                    // Toggle the condition for else branch
                    cond_stack.push((!matches, true));
                    if in_else {
                        // Nested else without endif - error but continue
                        eprintln!("mailx: {}: unexpected else", path);
                    }
                }
                continue;
            }
            "en" | "end" | "endi" | "endif" => {
                if cond_stack.pop().is_none() {
                    eprintln!("mailx: {}: unexpected endif", path);
                }
                continue;
            }
            _ => {}
        }

        // Check if we should execute this command
        let should_execute = cond_stack.iter().all(|(matches, _)| *matches);
        if !should_execute {
            continue;
        }

        // Execute command
        if let Err(e) = commands::execute_startup_command(line, vars) {
            eprintln!("mailx: {}: {}", path, e);
        }
    }

    // Warn about unclosed conditionals
    if !cond_stack.is_empty() {
        eprintln!("mailx: {}: missing endif", path);
    }
}

fn get_system_mailbox() -> String {
    // Check MAIL environment variable first (common extension)
    if let Ok(mail) = env::var("MAIL") {
        return mail;
    }

    let user = get_user();
    // Try common mailbox locations
    let paths = [
        format!("/var/mail/{}", user),
        format!("/var/spool/mail/{}", user),
        format!("/usr/spool/mail/{}", user),
    ];

    for path in &paths {
        if std::path::Path::new(path).exists() {
            return path.clone();
        }
    }

    // Default to /var/mail/user
    paths[0].clone()
}

fn get_mbox_path() -> String {
    env::var("MBOX").unwrap_or_else(|_| {
        let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/mbox", home)
    })
}

fn get_user() -> String {
    env::var("USER")
        .or_else(|_| env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string())
}
