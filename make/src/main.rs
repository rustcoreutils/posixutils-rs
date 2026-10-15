//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::atomic::Ordering::Relaxed;
use std::{env, fs, io, process};

use clap::Parser;
use gettextrs::gettext;

use posixutils_make::{
    config::Config,
    error_code::ErrorCode::{self, *},
    parser::{
        preprocessor::{ENV_MACROS, MAKEFILE_LIST},
        Makefile,
    },
    Make,
};

const MAKEFILE_NAME: [&str; 2] = ["makefile", "Makefile"];
const MAKEFILE_PATH: [&str; 2] = ["./makefile", "./Makefile"];

/// How many times the makefiles are read again because an included file was
/// remade.
const MAX_RESTARTS: usize = 16;

/// `MAKEFILE_PATH[i]` must stay `MAKEFILE_NAME[i]` prefixed with `./`; the two
/// are spelled out separately rather than derived, so pin the invariant here.
const _: () = {
    const fn is_dot_slash(path: &str, name: &str) -> bool {
        let (path, name) = (path.as_bytes(), name.as_bytes());
        if path.len() != name.len() + 2 || path[0] != b'.' || path[1] != b'/' {
            return false;
        }
        let mut i = 0;
        while i < name.len() {
            if path[i + 2] != name[i] {
                return false;
            }
            i += 1;
        }
        true
    }
    assert!(is_dot_slash(MAKEFILE_PATH[0], MAKEFILE_NAME[0]));
    assert!(is_dot_slash(MAKEFILE_PATH[1], MAKEFILE_NAME[1]));
};

/// make - maintain, update, and regenerate groups of programs
// todo: sort arguments
#[derive(Parser, Debug)]
#[command(version, about = gettext("make - maintain, update, and regenerate groups of programs"))]
struct Args {
    #[arg(
        short = 'C',
        long,
        allow_hyphen_values = true,
        help = "Change to DIRECTORY before doing anything"
    )]
    directory: Option<PathBuf>,

    #[arg(
        short = 'S',
        long,
        help = "Terminate make if error occurs. Default behavior"
    )]
    terminate: bool,

    #[arg(
        allow_hyphen_values = true,
        short = 'f',
        long,
        help = "Path to a makefile to parse (may be given multiple times; processed in order)"
    )]
    makefile: Vec<PathBuf>,

    #[arg(
        allow_hyphen_values = true,
        short = 'j',
        long,
        value_name = "maxjobs",
        overrides_with = "jobs",
        help = "Maximum number of targets to update concurrently (last value wins)"
    )]
    jobs: Option<usize>,

    #[arg(short = 'i', long, help = "Ignore errors in the recipe")]
    ignore: bool,

    #[arg(
        short = 'e',
        long,
        help = "Cause environment variables to override macro assignments within makefiles"
    )]
    env_macros: bool,

    #[arg(
        short = 'n',
        long,
        help = "Print commands to stdout and do not execute them"
    )]
    dry_run: bool,

    #[arg(short = 's', long, help = "Do not print recipe lines")]
    silent: bool,

    #[arg(
        short = 'q',
        long,
        help = "Return a zero exit value if the target file is up-to-date; otherwise, return an exit value of 1."
    )]
    quit: bool,

    #[arg(
        short = 'k',
        long,
        help = "Continue to update other targets that do not depend on the current target if a non-ignored error occur"
    )]
    keep_going: bool,

    #[arg(
        short = 'r',
        long,
        help = "Clear the suffix list and do not use the built-in rules"
    )]
    clear: bool,

    #[arg(
        short = 'p',
        long,
        help = "Write to standard output the complete set of macro definitions and target descriptions."
    )]
    print: bool,

    #[arg(
        short = 't',
        long,
        help = "If makefile should touch targets on execution"
    )]
    touch: bool,

    /// GNU make's option to stop announcing directory changes. This make
    /// never announces them, so there is nothing to turn off; it is accepted
    /// because libzstd's Makefile passes it to `$(MAKE)`.
    #[arg(long, hide = true)]
    no_print_directory: bool,

    #[arg(help = "Targets to build")]
    targets: Vec<OsString>,
}

/// Write the database `-p` asks for: POSIX 105395, "the complete set of macro
/// definitions and target descriptions".
fn print_database(make: &Make) {
    print!("{}", make.database());
}

/// The `-k` "target could not be remade" diagnostic, routed through `gettext`
/// so `LC_MESSAGES` can translate it (the English msgids reproduce the original
/// wording verbatim).
fn target_not_remade(target: &str) -> String {
    format!(
        "{}: {} {} {}",
        gettext("make"),
        gettext("Target"),
        target,
        gettext("not remade because of errors")
    )
}

/// The single-letter options a parent make may pass down in `MAKEFLAGS`.
///
/// `-f` and `-p` are left out by POSIX 105856 itself, `-C` and `-j` belong to
/// the invocation, and a letter this make does not implement -- GNU's `w`,
/// `B`, `R` and the rest -- is passed over when it arrives.
const INHERITED_LETTERS: &str = "inqksSetr";

/// Build the `MAKEFLAGS` value a sub-make should inherit.
///
/// POSIX 105856-105865: make's options, and its command-line macro
/// definitions "quoted in an implementation-defined manner such that when
/// MAKEFLAGS is read by another instance of the make command, the original
/// macro's value is recovered". The form is GNU make's, so that either make
/// reads the other's: the option letters, then `--`, then the macros, with a
/// blank or a backslash in a value escaped by a backslash.
fn makeflags_for_children(config: &Config, macros: &[String]) -> String {
    let mut out = String::new();
    for (flag, ch) in [
        (config.ignore, 'i'),
        (config.dry_run, 'n'),
        (config.silent, 's'),
        (config.quit, 'q'),
        (config.keep_going, 'k'),
        (config.terminate, 'S'),
        (config.env_macros, 'e'),
        (config.touch, 't'),
        (config.clear, 'r'),
    ] {
        if flag {
            out.push(ch);
        }
    }
    // The last definition of a name is the one in force; it alone is passed.
    let name_of = |m: &str| -> String {
        let name = m.split_once('=').map_or(m, |(name, _)| name);
        name.trim_end_matches([':', '?', '+', '!'])
            .trim()
            .to_string()
    };
    let mut seen = std::collections::HashSet::new();
    let mut macros: Vec<&String> = macros
        .iter()
        .rev()
        .filter(|m| name_of(m) != "MAKEFLAGS" && seen.insert(name_of(m)))
        .collect();
    macros.reverse();
    if macros.is_empty() {
        return out;
    }
    if !out.is_empty() {
        out.push(' ');
    }
    out.push_str("--");
    for definition in macros {
        out.push(' ');
        for c in definition.chars() {
            if matches!(c, ' ' | '\t' | '\\') {
                out.push('\\');
            }
            out.push(c);
        }
    }
    out
}

/// Split a `MAKEFLAGS` value into words at unescaped blanks, removing the
/// backslash from an escaped character.
fn makeflags_words(flags: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut word = String::new();
    let mut chars = flags.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => word.extend(chars.next()),
            ' ' | '\t' => {
                if !word.is_empty() {
                    words.push(std::mem::take(&mut word));
                }
            }
            _ => word.push(c),
        }
    }
    if !word.is_empty() {
        words.push(word);
    }
    words
}

/// The options and macros a `MAKEFLAGS` value carries, as arguments.
///
/// POSIX's forms are a first word of option letters (`kn`), option words
/// (`-k -n`) and macro definitions. A GNU make parent adds long options
/// (`--jobserver-auth=3,4`, `--no-print-directory`), option words with an
/// argument attached (`-j4`, `-Onone`, `-I/dir`), letters this make does not
/// have (`w`), and a `--` before its macros. A Debian build runs debian/rules
/// under GNU make and this make below it, so each of those reaches here; what
/// this make does not implement is passed over rather than refused, since
/// the parent's options were never addressed to it.
fn makeflags_args(flags: &str) -> Vec<OsString> {
    let known = |letters: &str| -> Option<OsString> {
        let kept: String = letters
            .chars()
            .filter(|c| INHERITED_LETTERS.contains(*c))
            .collect();
        (!kept.is_empty()).then(|| OsString::from(format!("-{kept}")))
    };

    let mut args = Vec::new();
    let mut macros_only = false;
    for (i, word) in makeflags_words(flags).iter().enumerate() {
        if macros_only || (word.contains('=') && !word.starts_with('-')) {
            if word.contains('=') {
                args.push(OsString::from(word));
            }
        } else if word == "--" {
            macros_only = true;
        } else if word.starts_with("--") {
            // A GNU long option: nothing this make implements.
        } else if let Some(letters) = word.strip_prefix('-') {
            // An option word. One whose first letter takes an argument
            // (`-j4`, `-Onone`) is dropped whole, so the argument is not read
            // as more letters.
            if letters.starts_with(|c: char| INHERITED_LETTERS.contains(c)) {
                args.extend(known(letters));
            }
        } else if i == 0 {
            args.extend(known(word));
        }
    }
    args
}

/// Build the effective argument vector, seeding options and macros from the
/// `MAKEFLAGS` environment variable ahead of the real command line, so that
/// the command line wins (POSIX 105839-105851: source 1 before source 2).
fn args_with_makeflags() -> Vec<OsString> {
    let mut args: Vec<OsString> = env::args_os().collect();
    let Ok(flags) = env::var("MAKEFLAGS") else {
        return args;
    };
    let tail = args.split_off(1);
    args.extend(makeflags_args(&flags));
    args.extend(tail);
    args
}

/// Read one makefile operand into a string, honoring `-` as standard input.
fn read_makefile(path: &Path) -> Result<String, ErrorCode> {
    if path == Path::new("-") {
        read_stdin()
    } else {
        fs::read_to_string(path).map_err(|err| IoError(err.kind()))
    }
}

/// Append `part` to `contents`, inserting a separating newline only between
/// non-empty parts. A trailing newline is never added, so a single empty
/// makefile stays empty (which the parser reports as "No targets").
fn append_part(contents: &mut String, part: &str) {
    if !contents.is_empty() && !contents.ends_with('\n') {
        contents.push('\n');
    }
    contents.push_str(part);
}

/// Parse the makefile(s). Multiple `-f` operands are concatenated in the order
/// given (POSIX); with none, the first default makefile found is used.
/// Command-line `macro=value` operands are appended last so they take
/// precedence over definitions in the makefile(s).
fn parse_makefile(
    paths: &[PathBuf],
    cmdline_macros: &[String],
    builtins: &[(String, String)],
) -> Result<Makefile, ErrorCode> {
    let mut contents = String::new();
    let mut names: Vec<String> = Vec::new();

    if paths.is_empty() {
        let default = MAKEFILE_PATH
            .iter()
            .map(Path::new)
            .find(|p| p.exists())
            .ok_or(NoMakefile)?;
        append_part(&mut contents, &read_makefile(default)?);
        names.push(
            default
                .to_string_lossy()
                .trim_start_matches("./")
                .to_string(),
        );
    } else {
        for path in paths {
            append_part(&mut contents, &read_makefile(path)?);
            names.push(path.to_string_lossy().into_owned());
        }
    }
    // GNU make's MAKEFILE_LIST starts with the makefiles named here; each
    // include adds its file.
    let mut builtins = builtins.to_vec();
    builtins.push((MAKEFILE_LIST.to_string(), names.join(" ")));

    // Command-line macros are *seeded*, not appended: a rule header is expanded
    // where it appears, so appending left `all: $(OBJ)` already expanded by the
    // time the operand was read (audit #71).
    let seeded: Vec<(String, String)> = cmdline_macros
        .iter()
        .filter_map(|def| def.split_once('='))
        .map(|(name, value)| {
            let name = name.trim_end_matches([':', '?', '+', '!']).trim();
            (name.to_string(), cmdline_value(value).to_string())
        })
        .filter(|(name, _)| !name.is_empty())
        .collect();

    match Makefile::parse_with_macros(&contents, &seeded, &builtins) {
        Ok(makefile) => Ok(makefile),
        Err(err) => Err(ErrorCode::ParserError { constraint: err }),
    }
}

/// The value of a command-line `macro=value` operand: as for a definition
/// in a makefile, the blanks after the `=` are not part of it (GNU make
/// reads it the same way). libzstd passes `LDFLAGS="$(LDFLAGS)"` to a
/// sub-make with a value that starts with a blank.
fn cmdline_value(value: &str) -> &str {
    value.trim_start_matches([' ', '\t'])
}

/// Reads the makefile from `stdin` until EOF (Ctrl + D)
fn read_stdin() -> Result<String, ErrorCode> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("make");

    let Args {
        directory,
        makefile,
        env_macros,
        ignore,
        dry_run,
        silent,
        quit,
        clear,
        touch,
        print,
        terminate,
        keep_going,
        jobs,
        no_print_directory: _,
        mut targets,
    } = plib::optarg::parse_from::<Args>(plib::optarg::keep_leading_equals::<Args>(
        args_with_makeflags(),
    ));

    let mut status_code = 0;

    // -j maxjobs: a non-positive value is unspecified; treat it (and absence)
    // as 1 (no parallelism).
    let jobs = jobs.filter(|&j| j >= 1).unwrap_or(1);

    // -C flag
    if let Some(dir) = directory {
        env::set_current_dir(dir)?;
    }

    let mut config = Config {
        ignore,
        dry_run,
        silent,
        touch,
        env_macros,
        quit,
        keep_going,
        clear,
        print,
        precious: false,
        jobs,
        terminate,
        ..Default::default()
    };

    if clear {
        config.suffixes.clear();
    }

    ENV_MACROS.store(env_macros, Relaxed);

    // Separate command-line `macro=value` operands from target operands
    // (POSIX SYNOPSIS allows them to be intermixed).
    let mut cmdline_macros: Vec<String> = Vec::new();
    targets.retain(|operand| {
        let operand = operand.to_string_lossy();
        if posixutils_make::parser::preprocessor::is_macro_definition(&operand) {
            cmdline_macros.push(operand.into_owned());
            false
        } else {
            true
        }
    });

    // POSIX 105856: make passes its options and command-line macros to
    // sub-makes through MAKEFLAGS. -f, -C and the target operands belong to
    // this invocation (audit #40). The value replaces whatever was
    // inherited, a GNU parent's jobserver included.
    env::set_var(
        "MAKEFLAGS",
        makeflags_for_children(&config, &cmdline_macros),
    );

    // POSIX 105866: "all of the make utility command line macro definitions
    // (except the MAKEFLAGS macro or the SHELL macro) shall be added to the
    // environment of make". Putting them there is also what makes them visible
    // to recipes, since a macro is exported only if make already inherited its
    // name -- the rule that keeps *makefile* macros from leaking (audit #50).
    for definition in &cmdline_macros {
        let Some((name, value)) = definition.split_once('=') else {
            continue;
        };
        let name = name.trim_end_matches([':', '?', '+', '!']).trim();
        if name.is_empty() || name == "MAKEFLAGS" || name == "SHELL" {
            continue;
        }
        env::set_var(name, cmdline_value(value));
    }

    // POSIX source 4: the macros the built-in inference rules refer to. They
    // are in force before the makefile is read, so the makefile can see them --
    // `$(CC)` used to expand to nothing because they were only added to the
    // finished `Make`. `-r` drops the built-in rules, and these with them.
    let builtins: Vec<(String, String)> = match clear {
        true => Vec::new(),
        false => posixutils_make::builtin_macros(),
    };

    // Read the makefiles, then bring the files they include up to date; if
    // any of those changed, read everything again (POSIX "delayed remaking",
    // 105587-105600). The bound only stops a rule that changes its own
    // makefile on every run from looping forever.
    let mut restarts = 0;
    let make = loop {
        let parsed = match parse_makefile(&makefile, &cmdline_macros, &builtins) {
            Ok(parsed) => parsed,
            Err(err) => {
                // -p with no usable makefile still has a database to show:
                // the built-in macros and inference rules. POSIX's own example
                // is `make -p -f /dev/null`, which has no targets and so does
                // not parse. Build an empty makefile to get the built-ins
                // seeded.
                if print {
                    let empty = Make::try_from((Makefile::default(), config)).map_err(|err| {
                        eprintln!("make: {err}");
                        err
                    })?;
                    print_database(&empty);
                    return Ok(());
                } else {
                    eprintln!("make: {}", err);
                    process::exit(err.into());
                }
            }
        };
        let includes = parsed.includes().to_vec();

        let mut make = Make::try_from((parsed, config.clone())).unwrap_or_else(|err| {
            eprintln!("make: {err}");
            process::exit(err.into());
        });

        match make.remake_includes(&includes) {
            Ok(true) if restarts < MAX_RESTARTS => restarts += 1,
            Ok(_) => break make,
            Err(err) => {
                eprintln!("make: {err}");
                process::exit(err.into());
            }
        }
    };

    // -p: POSIX does not say this suppresses the build, unlike -q, so the
    // dump is written and the targets are still made.
    if print {
        print_database(&make);
    }

    if targets.is_empty() {
        let target = make
            .first_target()
            .unwrap_or_else(|err| {
                eprintln!("make: {err}");
                process::exit(err.into());
            })
            .to_string()
            .into();

        targets.push(target);
    }

    let mut had_error = false;
    for target in targets {
        // A target is a filename, so it need not be valid UTF-8. Diagnose it
        // rather than panicking (audit #39); the rest of the crate is
        // String-based, so a non-UTF-8 operand cannot be built, but it can at
        // least be reported.
        let target = match target.into_string() {
            Ok(target) => target,
            Err(raw) => {
                eprintln!(
                    "make: {}: {}",
                    raw.to_string_lossy(),
                    gettext("target name is not valid text")
                );
                had_error = true;
                continue;
            }
        };
        match make.build_target(&target) {
            // -q asks a question and prints nothing: the answer is the exit
            // status. The build reports "not up to date" rather than calling
            // `process::exit` from a worker thread (audit #76), so the choice
            // of status lands here instead.
            Err(NotUpToDateError { .. }) if quit => {
                status_code = 1;
                break;
            }
            Ok(_) if quit => {}
            Ok(updated) => {
                if !updated {
                    println!(
                        "{}: `{}` {}",
                        gettext("make"),
                        target,
                        gettext("is up to date.")
                    );
                }
            }
            Err(err) => {
                eprintln!("make: {}", err);
                had_error = true;
                if keep_going {
                    // `-k`: report this target, then keep building the
                    // remaining (independent) command-line targets.
                    eprintln!("{}", target_not_remade(&target));
                } else {
                    status_code = err.into();
                    break;
                }
            }
        }
    }

    // Any non-ignored build failure must exit with a status > 1.
    if had_error && status_code == 0 {
        status_code = 2;
    }
    process::exit(status_code);
}
