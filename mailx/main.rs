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
        load_startup_files(&mut vars);
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

fn init_variables(vars: &mut Variables, _args: &Args) {
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
}

fn load_startup_files(vars: &mut Variables) {
    // Load user startup file
    let mailrc = env::var("MAILRC").unwrap_or_else(|_| {
        let home = env::var("HOME").unwrap_or_else(|_| ".".to_string());
        format!("{}/.mailrc", home)
    });

    if let Ok(content) = std::fs::read_to_string(&mailrc) {
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            // Execute commands from mailrc (limited set allowed)
            if let Err(e) = commands::execute_startup_command(line, vars) {
                eprintln!("mailx: {}: {}", mailrc, e);
            }
        }
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
