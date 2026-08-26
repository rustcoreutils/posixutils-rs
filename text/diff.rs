//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - Just a start with the core algorithm; -C and -U both need context output
// - Implement -r (recurse)
// - Research and implement -f alternate output format properly
//

mod diff_util;

use std::{fs, io, path::PathBuf};

use clap::Parser;
use diff_util::{
    common::{FormatOptions, OutputFormat},
    diff_exit_status::DiffExitStatus,
    dir_diff::DirDiff,
    file_diff::{FileDiff, Source},
    functions::check_existance,
};
use gettextrs::gettext;

/// diff - compare two files
#[derive(Parser, Clone)]
#[command(version, about = gettext("diff - compare two files"))]
struct Args {
    #[arg(short = 'b', long = "ignore-space-change", help = gettext("Cause EOL whitespace to be treated as blanks"))]
    ignore_eol_space: bool,

    #[arg(short, help = gettext("Output 3 lines of copied context"))]
    context3: bool,

    #[arg(short='C', value_parser = clap::value_parser!(u32), help = gettext("Output <N> lines of copied context"))]
    context: Option<u32>,

    #[arg(short, long, help = gettext("Produce output in a form suitable as input for the ed utility"))]
    ed: bool,

    #[arg(short, help = gettext("Produce output in an alternative form, similar in format to -e"))]
    fed: bool,

    #[arg(short, long, help = gettext("Apply diff recursively to files and directories of the same name"))]
    recurse: bool,

    #[arg(short, help = gettext("Output 3 lines of unified context"))]
    unified3: bool,

    #[arg(short='U', value_parser = clap::value_parser!(u32).range(0..), help = gettext("Output <N> lines of unified context"))]
    unified: Option<u32>,

    #[arg(help = gettext("First comparison file (or directory, if -r is specified)"))]
    file1: String,

    #[arg(short = 'L', long = "label", action = clap::ArgAction::Append, help = gettext("Use <LABEL> instead of the file name in the header; may be given twice"))]
    label: Vec<String>,

    #[arg(long, value_parser= clap::value_parser!(String), help = gettext("Label for second file"))]
    label2: Option<String>,

    #[arg(help = gettext("Second comparison file (or directory, if -r is specified)"))]
    file2: String,
}

impl Args {
    /// The output style, or `None` if the options named more than one.
    ///
    /// Resolving a conflict by precedence silently ignores an option the user
    /// asked for -- `diff -u -e` and `diff -e -u` both produced an ed script --
    /// so refuse instead, as GNU diff does.
    fn output_format(&self) -> Option<OutputFormat> {
        let context = self.context.or(if self.context3 { Some(3) } else { None });
        let unified = self.unified.or(if self.unified3 { Some(3) } else { None });

        let mut chosen = None;
        let mut set = |format| match chosen {
            None => {
                chosen = Some(format);
                true
            }
            Some(_) => false,
        };

        let ok = (!self.ed || set(OutputFormat::EditScript))
            && (!self.fed || set(OutputFormat::ForwardEditScript))
            && context.is_none_or(|n| set(OutputFormat::Context(n as usize)))
            && unified.is_none_or(|n| set(OutputFormat::Unified(n as usize)));

        ok.then(|| chosen.unwrap_or(OutputFormat::Default))
    }

    /// The two header labels, in file order.
    ///
    /// `-L` may be given twice, as in GNU diff; `--label2` is our own spelling
    /// of the second one.
    fn labels(&self) -> Result<(Option<String>, Option<String>), &'static str> {
        let mut labels = self.label.clone();
        if let Some(second) = &self.label2 {
            // --label2 is our own spelling of the second label, so it only
            // makes sense once the first one has been named.
            if labels.is_empty() {
                return Err("--label2 needs --label");
            }
            labels.push(second.clone());
        }
        if labels.len() > 2 {
            return Err("too many file label options");
        }
        let mut it = labels.into_iter();
        Ok((it.next(), it.next()))
    }
}

/// The option arguments from the command line, with the two file operands
/// removed.
///
/// POSIX specifies the per-file header `-r` prints as
/// `"diff %s %s %s\n", <diff_options>, <filename1>, <filename2>` where the
/// options are "as specified on the command line", so they have to come from
/// argv rather than from a canonical rendering of the parsed options. The two
/// operands are the last two positionals, so removing each one's last
/// occurrence -- second operand first -- takes the operands and not an option
/// value that happens to read the same, as in `diff -L a a b`.
fn option_arguments(file1: &str, file2: &str) -> Vec<String> {
    let mut argv: Vec<String> = std::env::args().skip(1).collect();
    if let Some(i) = argv.iter().rposition(|a| a == file2) {
        argv.remove(i);
        if let Some(j) = argv.iter().take(i).rposition(|a| a == file1) {
            argv.remove(j);
        }
    }
    argv
}

fn check_difference(args: Args) -> io::Result<DiffExitStatus> {
    let is_stdin1 = args.file1 == "-";
    let is_stdin2 = args.file2 == "-";

    // Cannot compare stdin to itself
    if is_stdin1 && is_stdin2 {
        eprintln!("diff: cannot compare stdin to itself");
        return Ok(DiffExitStatus::Trouble);
    }

    let Some(output_format) = args.output_format() else {
        eprintln!("diff: conflicting output style options");
        return Ok(DiffExitStatus::Trouble);
    };

    let (label1, label2) = match args.labels() {
        Ok(labels) => labels,
        Err(message) => {
            eprintln!("diff: {}", message);
            return Ok(DiffExitStatus::Trouble);
        }
    };

    let format_options = FormatOptions::new(args.ignore_eol_space, output_format, label1, label2);

    let path1 = PathBuf::from(&args.file1);
    let path2 = PathBuf::from(&args.file2);

    if !is_stdin1 && !check_existance(&path1)? {
        return Ok(DiffExitStatus::Trouble);
    }
    if !is_stdin2 && !check_existance(&path2)? {
        return Ok(DiffExitStatus::Trouble);
    }

    // Standard input is read into memory rather than spilled to a file. The
    // spill file had a predictable name in a world-writable directory and was
    // opened without O_EXCL, and it leaked on every early return below; none
    // of that bought anything, since both operands are read whole regardless.
    if is_stdin1 || is_stdin2 {
        let src1 = if is_stdin1 {
            Source::from_stdin()?
        } else {
            Source::from_path(path1)?
        };
        let src2 = if is_stdin2 {
            Source::from_stdin()?
        } else {
            Source::from_path(path2)?
        };
        return FileDiff::diff_sources(src1, src2, &format_options, None);
    }

    // The same file named twice has no differences; that is a normal result,
    // not an error. This reported "trouble" (exit 2) with no diagnostic at all.
    if path1 == path2 {
        return Ok(DiffExitStatus::NotDifferent);
    }

    let path1_is_file = fs::metadata(&path1)?.is_file();
    let path2_is_file = fs::metadata(&path2)?.is_file();

    if path1_is_file && path2_is_file {
        FileDiff::file_diff(path1, path2, &format_options, None)
    } else if !path1_is_file && !path2_is_file {
        let options = option_arguments(&args.file1, &args.file2);
        DirDiff::dir_diff(path1, path2, &format_options, args.recurse, &options)
    } else {
        FileDiff::file_dir_diff(path1, path2, &format_options)
    }
}

fn main() -> DiffExitStatus {
    // diff is routinely piped into head or less; without this a closed pipe is
    // a panic and exit 101 rather than death by SIGPIPE.
    plib::io::restore_sigpipe();
    plib::diag::init_locale("diff");

    let args = Args::parse();

    let result = check_difference(args);

    match result {
        Ok(diff_exit_status) => diff_exit_status,
        Err(error) => {
            eprintln!("diff: {}", error);

            DiffExitStatus::Trouble
        }
    }
}
